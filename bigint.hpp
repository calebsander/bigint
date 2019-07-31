#ifndef __BIGINT_H__
#define __BIGINT_H__

#include <cstddef>
#include <string>
#include <vector>

class BigInt {
	public:
		using uword_t = size_t;
		using sword_t = ptrdiff_t;

		BigInt();
		BigInt(const BigInt &);
		BigInt(BigInt &&);
		BigInt(uword_t);
		BigInt(sword_t);
		BigInt(const std::string &digits, uword_t radix = 10);
		~BigInt();

		BigInt &operator=(BigInt);

		// In-place arithmetic operations
		BigInt &invert();
		BigInt &negate();
		BigInt &operator&=(const BigInt &);
		BigInt &operator|=(const BigInt &);
		BigInt &operator^=(const BigInt &);
		BigInt &operator<<=(size_t);
		BigInt &operator>>=(size_t);
		BigInt &operator+=(const BigInt &);
		BigInt &operator-=(const BigInt &);
		BigInt &operator*=(const BigInt &);
		BigInt &operator/=(const BigInt &);
		BigInt &operator%=(const BigInt &);

		BigInt operator~() const;
		BigInt operator-() const;
		BigInt operator&(const BigInt &) const;
		BigInt operator|(const BigInt &) const;
		BigInt operator^(const BigInt &) const;
		BigInt operator<<(size_t) const;
		BigInt operator>>(size_t) const;
		BigInt operator+(const BigInt &) const;
		BigInt operator-(const BigInt &) const;
		BigInt operator*(const BigInt &) const;
		BigInt operator/(const BigInt &) const;
		BigInt operator%(const BigInt &) const;
		BigInt pow(size_t) const;

		int8_t cmp(const BigInt &) const;
		bool operator<(const BigInt &) const;
		bool operator<=(const BigInt &) const;
		bool operator==(const BigInt &) const;
		bool operator!=(const BigInt &) const;
		bool operator>=(const BigInt &) const;
		bool operator>(const BigInt &) const;

		std::string toString(uword_t radix = 10) const;

		explicit operator bool() const;

		bool isTrimmed() const; // for testing

	private:
		bool sign;
		std::vector<uword_t> words; // LE

		uword_t getSignWord() const;
		void trim() const;

		void divMod(const BigInt &other, BigInt *quotient);

		static void checkRadix(uword_t);
};

#endif // #ifndef __BIGINT_H__
