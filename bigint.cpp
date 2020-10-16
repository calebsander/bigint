#include "bigint.hpp"
#include <algorithm>
#include <stdexcept>

#define MAX_RADIX 36

class DigitLookup {
	public:
		DigitLookup() {
			uint8_t i = 0;
			while (i < 10) {
				const char c = '0' + i;
				valueDigits[i] = c;
				digitValues[static_cast<uint8_t>(c)] = i++;
			}
			while (i < MAX_RADIX) {
				const char upperC = 'A' - 10 + i;
				valueDigits[i] = upperC;
				digitValues[static_cast<uint8_t>(upperC)] =
				digitValues[static_cast<uint8_t>('a' - 'A' + upperC)] = i++;
			}
		}

		inline char getDigit(const size_t value) const {
			return valueDigits[value];
		}
		inline uint8_t getValue(const char digit) const {
			return digitValues[static_cast<uint8_t>(digit)];
		}

	private:
		char valueDigits[MAX_RADIX];
		uint8_t digitValues[1 << (sizeof(char) * 8)];
};
const DigitLookup lookup;

inline void checkRadix(BigInt::uword_t radix) {
	if (radix < 2 || radix > MAX_RADIX) throw std::out_of_range("Invalid radix");
}

const uint8_t WORD_BITS = sizeof(BigInt::uword_t) * 8;
constexpr uint8_t log2Const(uint8_t x) {
	return x == 1 ? 0 : log2Const(x >> 1) + 1;
}
const uint8_t LOG_WORD_BITS = log2Const(WORD_BITS);

inline size_t bitsToWords(size_t bits) {
	return (WORD_BITS - 1 + bits) >> LOG_WORD_BITS;
}

inline uint8_t log2Floor(const size_t x) {
	size_t bits;
	asm(
		"lzcnt %1, %0\n"
		"neg %0\n"
		"add %2, %0\n"
		: "=r"(bits)
		: "g"(x), "i"(WORD_BITS - 1)
		: "cc"
	);
	return static_cast<uint8_t>(bits);
}
inline uint8_t log2Ceil(const size_t x) {
	return log2Floor(x) + !!(x & (x - 1));
}

inline BigInt::uword_t ceilDiv(BigInt::uword_t x, const BigInt::uword_t y) {
	BigInt::uword_t remainder = 0;
	asm(
		"divq %2\n"
		: "+a"(x), "+d"(remainder)
		: "g"(y)
		: "cc"
	);
	return x + !!remainder;
}

inline void invertWord(BigInt::uword_t &word) {
	// clang and gcc are smart enough to emit a notq instead of an xorq
	word ^= static_cast<BigInt::uword_t>(-1);
}

inline int8_t makeSign(const bool positive) {
	return positive - !positive;
}

BigInt::BigInt() : sign(false) {}
BigInt::BigInt(const BigInt &) = default;
BigInt::BigInt(BigInt &&other)
	: sign(other.sign)
	, words(std::move(other.words))
	{ other.sign = false; }
BigInt::BigInt(const uword_t value) : sign(false) {
	if (value) words.push_back(value);
}
BigInt::BigInt(const sword_t value) : sign(value < 0) {
	const uword_t word = static_cast<uword_t>(value);
	if (word != getSignWord()) words.push_back(word);
}
BigInt::BigInt(const std::string &digits, const uword_t radix) : sign(false) {
	checkRadix(radix);

	auto it = digits.begin(), end = digits.end();
	bool negative = false;
	if (it < end) {
		const char firstChar = *it;
		if (firstChar == '-') negative = true;
		if (negative || firstChar == '+') ++it;
	}

	const uint8_t digitBits = log2Ceil(radix);
	words.resize(bitsToWords(digitBits * digits.size()));
	uword_t * const data = words.data();
	size_t setBits = 0;
	while (it != end) {
		uword_t carry = lookup.getValue(*it);
		setBits += digitBits;
		uword_t *word = data;
		for (size_t index = bitsToWords(setBits); index; index--) {
			asm(
				"mul %2\n"
				"add %3, %0\n"
				"adc $0, %1\n"
				: "+a"(*word), "=&d"(carry)
				: "r"(radix), "r"(carry)
				: "cc"
			);
			word++;
		}
		++it;
	}
	trim();
	if (negative) negate();
}
BigInt::~BigInt() = default;

BigInt &BigInt::operator=(BigInt other) {
	sign = other.sign;
	std::swap(words, other.words);
	return *this;
}

BigInt &BigInt::invert() {
	for (uword_t &word : words) invertWord(word);
	sign = !sign;
	return *this;
}
BigInt &BigInt::negate() {
	// If negative, may become one word longer
	if (sign) words.push_back(static_cast<BigInt::uword_t>(-1));
	else {
		// Special case for 0 -- sign does not change
		if (words.empty()) return *this;
	}

	sign = !sign;
	size_t wordsNegated = 0;
	uword_t *word = words.data();
	asm(
		"jmp 2f\n"
		"1:"
		"inc %0\n"
		"2:"
		"negq (%1, %0, 8)\n"
		"jz 1b\n"
		: "+r"(wordsNegated)
		: "r"(word)
		: "cc"
	);
	const uword_t * const endWord = &*words.end();
	for (word += wordsNegated + 1; word < endWord; word++) invertWord(*word);
	trimOnce();
	return *this;
}
BigInt &BigInt::operator&=(const BigInt &other) {
	size_t wordCount = words.size(), otherWordCount = other.words.size();
	size_t andWordCount;
	if (wordCount < otherWordCount) {
		if (sign) words.reserve(otherWordCount);
		else otherWordCount = wordCount;
		andWordCount = wordCount;
	}
	else {
		if (!other.sign) words.resize(otherWordCount);
		andWordCount = otherWordCount;
	}
	uword_t * const data = words.data();
	const uword_t * const otherData = other.words.data();
	size_t index;
	for (index = 0; index < andWordCount; index++) {
		data[index] &= otherData[index];
	}
	while (index < otherWordCount) {
		words.push_back(otherData[index++]);
	}
	sign &= other.sign;
	trim();
	return *this;
}
BigInt &BigInt::operator|=(const BigInt &other) {
	size_t wordCount = words.size(), otherWordCount = other.words.size();
	size_t orWordCount;
	if (wordCount < otherWordCount) {
		if (sign) otherWordCount = wordCount;
		else words.reserve(otherWordCount);
		orWordCount = wordCount;
	}
	else {
		if (other.sign) words.resize(otherWordCount);
		orWordCount = otherWordCount;
	}
	uword_t * const data = words.data();
	const uword_t * const otherData = other.words.data();
	size_t index;
	for (index = 0; index < orWordCount; index++) {
		data[index] |= otherData[index];
	}
	while (index < otherWordCount) {
		words.push_back(otherData[index++]);
	}
	sign |= other.sign;
	trim();
	return *this;
}
BigInt &BigInt::operator^=(const BigInt &other) {
	const size_t wordCount = words.size(), otherWordCount = other.words.size();
	size_t xorWordCount;
	if (wordCount < otherWordCount) {
		xorWordCount = wordCount;
		words.reserve(otherWordCount);
	}
	else xorWordCount = otherWordCount;
	uword_t * const data = words.data();
	const uword_t * const otherData = other.words.data();
	size_t index;
	for (index = 0; index < xorWordCount; index++) {
		data[index] ^= otherData[index];
	}
	if (index < otherWordCount) {
		if (sign) {
			do {
				words.push_back(~otherData[index++]);
			} while (index < otherWordCount);
		}
		else {
			do {
				words.push_back(otherData[index++]);
			} while (index < otherWordCount);
		}
	}
	else if (other.sign) {
		while (index < wordCount) invertWord(data[index++]);
	}
	sign ^= other.sign;
	trim();
	return *this;
}
BigInt &BigInt::operator<<=(const size_t bits) {
	if (!(bits && *this)) return *this; // number is unchanged

	size_t wordShift = bits >> LOG_WORD_BITS;
	const uint8_t bitShift = bits & (WORD_BITS - 1);
	size_t wordCount = words.size();
	words.resize(wordCount + wordShift + !!bitShift); // TODO: can this initialization be avoided?
	uword_t * const data = words.data();
	uword_t * const targetData = data + wordShift;
	if (bitShift) {
		uint8_t inverseBitShift = WORD_BITS - bitShift;
		uword_t previousWord = getSignWord();
		while (wordCount) {
			const uword_t highBits = previousWord << bitShift;
			previousWord = data[--wordCount];
			targetData[wordCount + 1] = highBits | previousWord >> inverseBitShift;
		}
		targetData[0] = previousWord << bitShift;
		trimOnce();
	}
	else {
		while (wordCount) {
			--wordCount;
			targetData[wordCount] = data[wordCount];
		}
	}
	while (wordShift) data[--wordShift] = 0;
	return *this;
}
BigInt &BigInt::operator>>=(const size_t bits) {
	if (!bits) return *this;

	const size_t wordCount = words.size();
	const size_t wordShift = bits >> LOG_WORD_BITS;
	if (wordShift >= wordCount) {
		// Handles cases when highest word moves right of the decimal point
		// or wordCount is 0
		words.clear();
		return *this;
	}

	uword_t * const data = words.data();
	const uint8_t bitShift = bits & (WORD_BITS - 1);
	if (bitShift) {
		const uint8_t inverseBitShift = WORD_BITS - bitShift;
		size_t sourceIndex = wordShift, targetIndex = 0;
		uword_t previousWord = data[sourceIndex++];
		while (sourceIndex < wordCount) {
			const uword_t lowBits = previousWord >> bitShift;
			previousWord = data[sourceIndex++];
			data[targetIndex++] = previousWord << inverseBitShift | lowBits;
		}
		const uword_t signWord = getSignWord();
		const uword_t newHighWord =
			signWord << inverseBitShift | previousWord >> bitShift;
		if (newHighWord != signWord) data[targetIndex++] = newHighWord;
		words.resize(targetIndex);
	}
	else {
		uword_t * const sourceData = data + wordShift;
		const size_t newWordCount = wordCount - wordShift;
		for (size_t targetIndex = 0; targetIndex < newWordCount; targetIndex++) {
			data[targetIndex] = sourceData[targetIndex];
		}
		words.resize(newWordCount);
	}
	return *this;
}
#define CARRY_OPERATION(op) { \
	size_t wordCount = words.size(), otherWordCount = other.words.size(); \
	const size_t maxCount = \
		(wordCount > otherWordCount ? wordCount : otherWordCount) + 1; \
	words.resize(maxCount, getSignWord()); \
	wordCount = maxCount - otherWordCount; \
	const uword_t signWord = other.getSignWord(); \
	uword_t * const data = words.data(); \
	const uword_t * const otherData = other.words.data(); \
	size_t index = 0; \
	uword_t temp; \
	bool carry; \
	asm( \
		"test %2, %2\n" /* also clears carry flag */ \
		"jz 2f\n" \
		"1:" \
		"mov (%5, %0, 8), %3\n" \
		op " (%6, %0, 8), %3\n" \
		"mov %3, (%5, %0, 8)\n" \
		"inc %0\n" \
		"dec %2\n" \
		"jnz 1b\n" \
		/* TODO: skip the remaining words if other is unsigned and there is no carry */ \
		"2:" \
		op " %7, (%5, %0, 8)\n" \
		"inc %0\n" \
		"dec %1\n" \
		"jnz 2b\n" \
		"setc %4\n" \
		: "+r"(index), "+r"(wordCount), "+r"(otherWordCount), "=&r"(temp), "=g"(carry) \
		: "r"(data), "r"(otherData), "r"(signWord) \
		: "cc" \
	); \
	sign ^= other.sign ^ carry; \
	trim(); \
	return *this; \
}
BigInt &BigInt::operator+=(const BigInt &other) {
	CARRY_OPERATION("adc");
}
BigInt &BigInt::operator-=(const BigInt &other) {
	CARRY_OPERATION("sbb");
}
BigInt &BigInt::operator*=(const BigInt &other) {
	return *this = *this * other;
}
BigInt &BigInt::operator/=(const BigInt &other) {
	BigInt quotient;
	divMod(other, &quotient);
	return *this = quotient;
}
BigInt &BigInt::operator%=(const BigInt &other) {
	divMod(other, nullptr);
	return *this;
}

BigInt BigInt::operator~() const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this).invert();
}
BigInt BigInt::operator-() const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this).negate();
}
BigInt BigInt::operator&(const BigInt &other) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) &= other;
}
BigInt BigInt::operator|(const BigInt &other) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) |= other;
}
BigInt BigInt::operator^(const BigInt &other) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) ^= other;
}
BigInt BigInt::operator<<(const size_t bits) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) <<= bits;
}
BigInt BigInt::operator>>(const size_t bits) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) >>= bits;
}
BigInt BigInt::operator+(const BigInt &other) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) += other;
}
BigInt BigInt::operator-(const BigInt &other) const {
	// TODO: optimize to avoid traversing words twice
	return BigInt(*this) -= other;
}
BigInt BigInt::operator*(const BigInt &other) const {
	const BigInt &positiveThis = sign ? -*this : *this;
	const BigInt &positiveOther = other.sign ? -other : other;
	size_t otherWordCount = positiveOther.words.size();
	const size_t wordCount = positiveThis.words.size();
	BigInt result;
	result.words.reserve(wordCount + otherWordCount);
	result.words.resize(wordCount);
	const uword_t * const thisWords = positiveThis.words.data();
	const uword_t *otherWord = positiveOther.words.data();
	uword_t *resultWordStart = result.words.data();
	for (; otherWordCount; otherWordCount--) {
		const uword_t multiplier = *(otherWord++);
		uword_t carry = 0;
		for (size_t thisIndex = 0; thisIndex < wordCount; thisIndex++) {
			uword_t addWord;
			asm(
				"mul %4\n"
				"add %5, %0\n"
				"adc $0, %2\n"
				"add %0, %1\n"
				"adc $0, %2\n"
				: "=a"(addWord), "+m"(resultWordStart[thisIndex]), "=&d"(carry)
				: "a"(thisWords[thisIndex]), "r"(multiplier), "r"(carry)
				: "cc"
			);
		}
		result.words.push_back(carry);
		resultWordStart++;
	}
	result.trim();
	if (sign ^ other.sign) result.negate();
	return result;
}
BigInt BigInt::operator/(const BigInt &other) const {
	return BigInt(*this) /= other;
}
BigInt BigInt::operator%(const BigInt &other) const {
	return BigInt(*this) %= other;
}
BigInt BigInt::pow(size_t power) const {
	BigInt result(static_cast<uword_t>(1));
	if (power) {
		BigInt multiplier(*this);
		for (;;) {
			if (power & 1) result *= multiplier;
			if (!(power >>= 1)) break;

			multiplier *= multiplier;
		}
	}
	return result;
}

int8_t BigInt::cmp(const BigInt &other) const {
	const int8_t signDiff = other.sign - sign;
	if (signDiff) return signDiff;

	size_t wordCount = words.size();
	const size_t otherWordCount = other.words.size();
	if (wordCount > otherWordCount) return makeSign(!sign);
	if (wordCount < otherWordCount) return makeSign(sign);

	const uword_t * const data = words.data();
	const uword_t * const otherData = other.words.data();
	while (wordCount) {
		--wordCount;
		const uword_t word = data[wordCount], otherWord = otherData[wordCount];
		if (word > otherWord) return +1;
		if (word < otherWord) return -1;
	}

	return 0;
}
bool BigInt::operator<(const BigInt &other) const {
	return cmp(other) < 0;
}
bool BigInt::operator<=(const BigInt &other) const {
	return !(*this > other);
}
bool BigInt::operator==(const BigInt &other) const {
	return !cmp(other);
}
bool BigInt::operator!=(const BigInt &other) const {
	return !(*this == other);
}
bool BigInt::operator>(const BigInt &other) const {
	return cmp(other) > 0;
}
bool BigInt::operator>=(const BigInt &other) const {
	return !(*this < other);
}

std::string BigInt::toString(const uword_t radix) const {
	checkRadix(radix);

	if (!*this) return "0"; // ensure at least one digit is returned

	BigInt copy(*this);
	if (sign) copy.negate();
	uword_t * const data = copy.words.data();
	size_t wordCount = copy.words.size();
	std::string result;
	result.reserve(sign + ceilDiv(wordCount << LOG_WORD_BITS, log2Floor(radix)));
	do {
		size_t wordIndex = wordCount;
		wordCount = 0;
		uword_t remainder = 0;
		uword_t *word = data + wordIndex;
		do {
			word--;
			asm(
				"divq %2\n"
				: "+a"(*word), "+d"(remainder)
				: "r"(radix)
				: "cc"
			);
			if (!wordCount && *word) wordCount = wordIndex;
		} while (--wordIndex);
		result += lookup.getDigit(remainder);
	} while (wordCount);
	if (sign) result += '-';
	std::reverse(result.begin(), result.end());
	return result;
}

BigInt::operator bool() const {
	return sign || words.size();
}

bool BigInt::isTrimmed() const {
	return words.empty() || words.back() != getSignWord();
}

inline BigInt::uword_t BigInt::getSignWord() const {
	return -static_cast<uword_t>(sign);
}
inline void BigInt::trim() {
	size_t wordCount = words.size();
	const uword_t signWord = getSignWord();
	const uword_t *word = &words.back();
	while (wordCount && *(word--) == signWord) wordCount--;
	words.resize(wordCount);
}
inline void BigInt::trimOnce() {
	if (words.back() == getSignWord()) words.pop_back();
}

void BigInt::divMod(const BigInt &other, BigInt *quotient) {
	// TODO: handle signedness
	if (quotient) *quotient = BigInt();
	if (other > *this) return;

	// TODO: operate on words instead of bits
	const size_t wordCount = words.size(), otherWordCount = other.words.size();
	const size_t shiftWords = wordCount + 1 - otherWordCount;
	size_t shiftBits = (shiftWords << LOG_WORD_BITS) - 1;
	BigInt otherCopy = other << shiftBits;
	uword_t * quotientData;
	if (quotient) {
		quotient->words.resize(shiftWords);
		quotientData = quotient->words.data();
	}
	for (;;) {
		if (*this >= otherCopy) {
			*this -= otherCopy;
			if (quotient) {
				quotientData[shiftBits >> LOG_WORD_BITS] |=
					static_cast<uword_t>(1) << (shiftBits & (WORD_BITS - 1));
			}
		}
		if (!shiftBits) break;

		shiftBits--;
		otherCopy >>= 1;
	}
	if (quotient) quotient->trim();
}
