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
		uint8_t getValue(const char digit) const {
			return digitValues[static_cast<uint8_t>(digit)];
		}

	private:
		char valueDigits[MAX_RADIX];
		uint8_t digitValues[1 << (sizeof(char) << 3)];
};
const DigitLookup lookup;

const uint8_t WORD_BITS = sizeof(BigInt::uword_t) << 3;
constexpr uint8_t log2Const(unsigned x) {
	return x == 1 ? 0 : log2Const(x >> 1) + 1;
}
const uint8_t LOG_WORD_BITS = log2Const(WORD_BITS);

template <typename I>
typename std::enable_if<std::is_unsigned<I>::value, I>::type
inline log2Floor(const I x) {
	I bits;
	asm(
		"lzcnt %1, %0\n"
		"neg %0\n"
		"add %2, %0\n"
		: "=r"(bits)
		: "g"(x), "i"(WORD_BITS - 1)
		: "cc"
	);
	return bits;
}

template <typename I>
typename std::enable_if<std::is_unsigned<I>::value, I>::type
inline log2Ceil(const I x) {
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
	if (value != getSignWord()) words.push_back(static_cast<uword_t>(value));
}
BigInt::BigInt(const std::string &digits, const uword_t radix) : sign(false) {
	checkRadix(radix);

	auto it = digits.begin(), end = digits.end();
	const bool negative = digits.size() && *it == '-';
	if (negative) ++it;

	const size_t wordCount =
		(WORD_BITS - 1 + log2Ceil(radix) * digits.size()) >> LOG_WORD_BITS;
	words.resize(wordCount);
	uword_t * const data = words.data();
	while (it != end) {
		uword_t carry = lookup.getValue(*it);
		uword_t *word = data;
		for (size_t index = wordCount; index; index--) {
			asm(
				"mul %2\n"
				"add %1, %0\n"
				"mov %%rdx, %1\n"
				"adc $0, %1\n"
				: "+a"(*word), "+r"(carry)
				: "r"(radix)
				: "rdx", "cc"
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
	uword_t * const data = words.data();
	const size_t wordCount = words.size();
	for (size_t index = 0; index < wordCount; index++) {
		data[index] ^= (uword_t) -1;
	}
	sign = !sign;
	return *this;
}
BigInt &BigInt::negate() {
	if (!*this) return *this;

	words.push_back(getSignWord());
	sign = !sign;
	size_t wordCount = words.size();
	uword_t *word = words.data();
	asm(
		"jmp 2f\n"
		"1:"
		"add %2, %0\n"
		"2:"
		"dec %1\n"
		"negq (%0)\n"
		"jz 1b\n"
		: "+r"(word), "+r"(wordCount)
		: "i"(sizeof(*word))
		: "cc"
	);
	if (wordCount) {
		words.pop_back();
		if (--wordCount) {
			asm(
				"1:"
				"add %2, %0\n"
				"notq (%0)\n"
				"dec %1\n"
				"ja 1b\n"
				:
				: "r"(word), "r"(wordCount), "i"(sizeof(*word))
				: "cc"
			);
		}
	}
	return *this;
}
BigInt &BigInt::operator&=(const BigInt &other) {
	size_t wordCount = words.size(), otherWordCount = other.words.size();
	size_t andWordCount;
	if (wordCount < otherWordCount) {
		if (sign) words.resize(otherWordCount); // TODO: can this initialization be avoided?
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
		data[index] = otherData[index];
		index++;
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
		else words.resize(otherWordCount); // TODO: can this initialization be avoided?
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
		data[index] = otherData[index];
		index++;
	}
	sign |= other.sign;
	trim();
	return *this;
}
BigInt &BigInt::operator<<=(const size_t bits) {
	if (!bits) return *this;

	const size_t wordShift = bits >> LOG_WORD_BITS;
	const uint8_t bitShift = bits & (WORD_BITS - 1);
	uint8_t inverseBitShift;
	uword_t signWord, newHighWord;
	bool extraWord;
	size_t wordCount = words.size();
	size_t newWordCount = wordCount + wordShift;
	if (bitShift) {
		inverseBitShift = WORD_BITS - bitShift;
		signWord = getSignWord();
		newHighWord = signWord << bitShift;
		if (wordCount) newHighWord |= words.back() >> inverseBitShift;
		extraWord = newHighWord != signWord;
		newWordCount += extraWord;
	}
	words.resize(newWordCount); // TODO: can this initialization be avoided?
	uword_t * const data = words.data();
	if (bitShift) {
		size_t targetIndex = newWordCount - 1;
		if (extraWord) {
			data[targetIndex] = newHighWord;
			if (targetIndex) targetIndex--;
		}
		if (wordCount) {
			wordCount--;
			while (wordCount) {
				uword_t highBits = data[wordCount--] << bitShift;
				data[targetIndex--] = highBits | data[wordCount] >> inverseBitShift;
			}
			data[targetIndex] = data[0] << bitShift;
		}
		while (targetIndex) data[--targetIndex] = 0;
	}
	else {
		while (wordCount) data[--newWordCount] = data[--wordCount];
		while (newWordCount) data[--newWordCount] = 0;
	}
	return *this;
}
BigInt &BigInt::operator>>=(const size_t bits) {
	throw "Unimplemented";
	return *this;
}
BigInt &BigInt::operator+=(const BigInt &other) {
	size_t wordCount = words.size(), otherWordCount = other.words.size();
	const size_t maxCount =
		(wordCount > otherWordCount ? wordCount : otherWordCount) + 1;
	words.reserve(maxCount);
	uword_t signWord = getSignWord();
	while (wordCount < maxCount) {
		words.push_back(signWord);
		wordCount++;
	}
	wordCount = maxCount - otherWordCount;
	signWord = other.getSignWord();
	uword_t * const word = words.data();
	const uword_t * const otherWord = other.words.data();
	size_t index = 0;
	bool carry;
	asm(
		"test %2, %2\n"
		"jz 2f\n"
		"1:"
		"mov (%4, %0, 8), %%rax\n"
		"adc (%5, %0, 8), %%rax\n"
		"mov %%rax, (%4, %0, 8)\n"
		"inc %0\n"
		"dec %2\n"
		"jnz 1b\n"

		"2:"
		"adc %6, (%4, %0, 8)\n"
		"inc %0\n"
		"dec %1\n"
		"jnz 2b\n"
		"setc %3\n"
		: "+r"(index), "+r"(wordCount), "+r"(otherWordCount), "=g"(carry)
		: "r"(word), "r"(otherWord), "r"(signWord)
		: "rax", "cc"
	);
	sign ^= other.sign ^ carry;
	trim();
	return *this;
}
BigInt &BigInt::operator-=(const BigInt &other) {
	// TODO: optimize to avoid traversing words twice
	return *this += -other;
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
	// TODO: optimize to avoid traversing words three times
	return BigInt(*this) -= other;
}

int8_t BigInt::cmp(const BigInt &other) const {
	const int8_t signDiff = other.sign - sign;
	if (signDiff) return signDiff;

	size_t wordCount = words.size();
	const ptrdiff_t wordCountCmp = wordCount - other.words.size();
	if (wordCountCmp) return makeSign((wordCountCmp < 0) ^ sign);

	while (wordCount) {
		--wordCount;
		const size_t word = words[wordCount], otherWord = other.words[wordCount];
		if (word > otherWord) return +1;
		if (word < otherWord) return -1;
	}

	return 0;
}
inline bool BigInt::operator<(const BigInt &other) const {
	return cmp(other) < 0;
}
inline bool BigInt::operator<=(const BigInt &other) const {
	return cmp(other) <= 0;
}
inline bool BigInt::operator==(const BigInt &other) const {
	return !cmp(other);
}
inline bool BigInt::operator!=(const BigInt &other) const {
	return !!cmp(other);
}
inline bool BigInt::operator>(const BigInt &other) const {
	return cmp(other) > 0;
}
inline bool BigInt::operator>=(const BigInt &other) const {
	return cmp(other) >= 0;
}

std::string BigInt::toString(const uword_t radix) const {
	checkRadix(radix);

	const size_t wordsCount = words.size();
	if (!wordsCount) return sign ? "-1" : "0";

	BigInt copy(*this);
	if (sign) copy.negate();
	uword_t * const data = copy.words.data();
	size_t highestNonzeroWord = wordsCount;
	std::string result;
	result.reserve(sign + ceilDiv(wordsCount << LOG_WORD_BITS, log2Floor(radix)));
	for (;;) {
		size_t wordIndex = highestNonzeroWord, newHighestNonzeroWord = 0;
		uword_t remainder = 0;
		uword_t *word = data + wordIndex;
		do {
			word--;
			asm(
				"divq %2\n"
				: "+a"(*word), "+d"(remainder)
				: "g"(radix)
				: "cc"
			);
			if (!newHighestNonzeroWord && *word) newHighestNonzeroWord = wordIndex;
		} while (--wordIndex);
		result += lookup.getDigit(remainder);
		if (!newHighestNonzeroWord) break;

		highestNonzeroWord = newHighestNonzeroWord;
	}
	if (sign) result += '-';
	std::reverse(result.begin(), result.end());
	return result;
}

inline BigInt::operator bool() const {
	return sign || words.size();
}

bool BigInt::isTrimmed() const {
	return !(words.size() && words.back() == getSignWord());
}

inline BigInt::uword_t BigInt::getSignWord() const {
	return -static_cast<uword_t>(sign);
}
inline void BigInt::trim() const {
	size_t wordCount = words.size();
	const uword_t signWord = getSignWord();
	const uword_t *word = &words.back();
	while (wordCount && *(word--) == signWord) wordCount--;
	const_cast<std::vector<uword_t> &>(words).resize(wordCount);
}

inline void BigInt::checkRadix(uword_t radix) {
	if (radix < 2 || radix > MAX_RADIX) throw std::out_of_range("Invalid radix");
}
