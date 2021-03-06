#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#include "bigint.hpp"

std::string builtinToString(const BigInt::sword_t n) {
	std::ostringstream stream;
	stream << n;
	return stream.str();
}

int8_t signum(const BigInt::sword_t n) {
	return (n > 0) - (n < 0);
}

TEST_CASE("toString") {
	// Test small values
	for (BigInt::sword_t i = -100000; i < 100000; i++) {
		CHECK(BigInt(i).toString() == builtinToString(i));
	}

	// Test multi-word numbers
	BigInt bigPrime("FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF", 16);
	CHECK(bigPrime.toString(2) == "11111111111111111111111111111111111111111111111111111111111111111100100100001111110110101010001000100001011010001100001000110100110001001100011001100010100010111000000011011100000111001101000100101001000000100100111000001000100010100110011111001100011101000000001000001011101111101010011000111011000100111001101100100010010100010100101000001000011110011000111000110100000001001101110111101111100101010001100110110011110011010011101001000011000110110011000000101011000010100110110111110010010111110001010000110111010011111110000100110101011011010110110101010001110000100100010111100100100001011011010101110110011000100101111001111110110001101111010001001100010000101110100110100110001101111110110101101011000010111111111101011100101101101111010000000110101101111110110111101110001110000110101111111011010110101000100110011111101001011010111010011111001001000001000101111100010010110001111111100110010010010010100001100110010100011110110011100100010110110011110111000010000000000111110010111000101000010110001110111111000001011001100011011010010010000011011000011100010101011101001110011010011010010001011000111111101010001111110100100100110011110101111110000011011001010101110100100011110111001010001110101101100101100001110001100010111100110101011000100000100001010101001010111011100111101101010100101001000001110111000010010110100101100110110101100111000011000011010101001110010010101011110010011000000001001111000101110100011011000000100011001010000110000010000101111100001100101001000001011110010001100010111000110110110011100011101111100011100111100111011100101100000110000000111010000110000000111001101100100111100000111010001011101100000001111010001010001111101101011100010101011101111100000110111101001100010100101100100111011110001010111100101111110110100101010101100000010111000110000011100110010101010010010111110011101010100101010110101011100101000101011101001000100110000110001001100011111010000001010001000000010101011100101000111001011010100010101010110010101010011010001111111111111111111111111111111111111111111111111111111111111111");
	CHECK(bigPrime.toString() == "32317006071311007300338913926423828248817941241140239112842009751400741706634354222619689417363569347117901737909704191754605873209195028853758986185622153212175412514901774520270235796078236248884246189477587641105928646099411723245426622522193230540919037680524235519125679715870117001058055877651038861847280257976054903569732561526167081339361799541336476559160368317896729073178384589680639671900977202194168647225871031411336429319536193471636533209717077448227988588565369208645296636077250268955505928362751121174096972998068410554359584866583291642136218231078990999448652468262416972035911852507045361090559");
	CHECK(bigPrime.toString(16) == "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF");
	CHECK(bigPrime.toString(36) == "1MVK6EPI7TT884KCU5JU26LWKIOF1TM5SAEM30M4VKWZ0C3VC5D0YP8A1RSS7LG4I3ZUMR0H2T3UU5P1FDP4HZH2S28BK37P1C76N6I6W8263PNG99KC0Z3CMIETAC6WNXKQ8M4GJBXWHULM9381DF7SUBBPP27R0ABILRKX07B5WIGBHM7CNBY0K934HT4LM2XTJOOBDQ0LTFLC8W3CFE68T5SPRDAUSGMPPRAZNT14CIRIIWZKYZU5TU6EEQR36DSLAMPPDMPEWJN8E8SLFZVKTL1VCM6FUG7DI30XWXG0K3R37S7QF58DQSBN94QKK6CKZ0T1U2UHYES4QERDLSI2JB6LLECESZ9AQ3TV1IKF6EPE1DZQ1KWC0B42BGG2X45RWRK2LVMDB");
	CHECK_THROWS_WITH(bigPrime.toString(1), "Invalid radix");
	CHECK_THROWS_WITH(bigPrime.toString(37), "Invalid radix");
}

TEST_CASE("bitwise negate") {
	// Test 0-word numbers
	BigInt value = ~BigInt();
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "-1");
	value = ~BigInt((BigInt::sword_t) -1);
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");

	// Test 1-word number
	value = BigInt("fedcba9876543210", 16);
	value.invert();
	CHECK(value.isTrimmed());
	CHECK(value.toString(16) == "-FEDCBA9876543211");
	value.invert();
	CHECK(value.isTrimmed());
	CHECK(value.toString(16) == "FEDCBA9876543210");

	// Test multi-word number
	value = BigInt("12345678901234567890123456789012345678901234567890");
	CHECK(value.isTrimmed());
	CHECK((~value).toString() == "-12345678901234567890123456789012345678901234567891");
}

TEST_CASE("bitwise and") {
	// Test trivial ands
	BigInt value("12345678901234567890123456789012345678901234567890");
	BigInt result = value & BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "0");
	result = value & ~BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == value.toString());

	// Test ands with the same number of words
	result = value & value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == value.toString());
	result = value & ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "0");
	result = ~value & value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "0");
	result = ~value & ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == (~value).toString());

	// Test ands with fewer words
	BigInt shorter("123456789012345678901234567890");
	result = value & shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "79266869146740925354769058514");
	result = value & ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "12345678901234567890044189919865604753546465509376");
	result = ~value & shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "44189919865604753546465509376");
	result = ~value & ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-12345678901234567890167646708877950432447700077267");

	// Test ands with more words
	BigInt longer("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890");
	result = value & longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "12059328684042537090238959080876447469613543787218");
	result = value & ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "286350217192030799884497708135898209287690780672");
	result = ~value & longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "123456789012345678901234567890123456789000286350217192030799884497708135898209287690780672");
	result = ~value & ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-123456789012345678901234567890123456789012632029118426598690007954497148243888188925348563");
}

TEST_CASE("bitwise or") {
	// Test trivial ors
	BigInt value("12345678901234567890123456789012345678901234567890");
	BigInt result = value | BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == value.toString());
	result = value | ~BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-1");

	// Test ors with the same number of words
	result = value | value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == value.toString());
	result = value | ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-1");
	result = ~value | value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-1");
	result = ~value | ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == (~value).toString());

	// Test ors with fewer words
	BigInt shorter("123456789012345678901234567890");
	result = value | shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "12345678901234567890167646708877950432447700077266");
	result = value | ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-44189919865604753546465509377");
	result = ~value | shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-12345678901234567890044189919865604753546465509377");
	result = ~value | ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-79266869146740925354769058515");

	// Test ors with more words
	BigInt longer("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890");
	result = value | longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "123456789012345678901234567890123456789012632029118426598690007954497148243888188925348562");
	result = value | ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-123456789012345678901234567890123456789000286350217192030799884497708135898209287690780673");
	result = ~value | longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-286350217192030799884497708135898209287690780673");
	result = ~value | ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-12059328684042537090238959080876447469613543787219");
}

TEST_CASE("bitwise xor") {
	// Test trivial xors
	BigInt value("12345678901234567890123456789012345678901234567890");
	BigInt result = value ^ BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == value.toString());
	result = value ^ ~BigInt();
	CHECK(result.isTrimmed());
	CHECK(result.toString() == (~value).toString());

	// Test xors with the same number of words
	result = value ^ value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "0");
	result = value ^ ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-1");
	result = ~value ^ value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-1");
	result = ~value ^ ~value;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "0");

	// Test xors with fewer words
	BigInt shorter("123456789012345678901234567890");
	result = value ^ shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "12345678901234567890088379839731209507092931018752");
	result = value ^ ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-12345678901234567890088379839731209507092931018753");
	result = ~value ^ shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-12345678901234567890088379839731209507092931018753");
	result = ~value ^ ~shorter;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "12345678901234567890088379839731209507092931018752");

	// Test xors with more words
	BigInt longer("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890");
	result = value ^ longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "123456789012345678901234567890123456789000572700434384061599768995416271796418575381561344");
	result = value ^ ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-123456789012345678901234567890123456789000572700434384061599768995416271796418575381561345");
	result = ~value ^ longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "-123456789012345678901234567890123456789000572700434384061599768995416271796418575381561345");
	result = ~value ^ ~longer;
	CHECK(result.isTrimmed());
	CHECK(result.toString() == "123456789012345678901234567890123456789000572700434384061599768995416271796418575381561344");
}

TEST_CASE("left-shift") {
	// Test 0-word numbers
	BigInt zero;
	BigInt minusOne = ~zero;
	for (int i = 0; i < 1000; i++) {
		BigInt value = zero << i;
		CHECK(value.isTrimmed());
		CHECK(value.toString() == "0");
		value = minusOne << i;
		CHECK(value.isTrimmed());
		CHECK(value.toString(2) == "-1" + std::string(i, '0'));
	}

	// Test partial-word numbers
	BigInt positive((BigInt::sword_t) 1023);
	BigInt negative = -positive;
	for (int i = 0; i < 1000; i++) {
		BigInt value = positive << i;
		CHECK(value.isTrimmed());
		CHECK(value.toString(2) == "1111111111" + std::string(i, '0'));
		value = negative << i;
		CHECK(value.isTrimmed());
		CHECK(value.toString(2) == "-1111111111" + std::string(i, '0'));
	}

	// Test numbers with 1 or more words
	positive = BigInt();
	for (int words = 1; words < 10; words++) {
		positive <<= sizeof(BigInt::uword_t) * 8;
		positive |= BigInt((BigInt::uword_t) -1);
		negative = -positive;
		for (int i = 0; i < 1000; i++) {
			BigInt value = positive << i;
			std::string expectedString =
				std::string(sizeof(BigInt::uword_t) * 8 * words, '1') + std::string(i, '0');
			CHECK(value.isTrimmed());
			CHECK(value.toString(2) == expectedString);
			value = negative << i;
			CHECK(value.isTrimmed());
			CHECK(value.toString(2) == '-' + expectedString);
		}
	}
}

TEST_CASE("right-shift") {
	BigInt positive("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890");
	std::string positiveString = positive.toString(2);
	size_t stringLength = positiveString.size();
	// Ensure negative number is a power of 2 to avoid flooring issues
	BigInt negative = -BigInt((BigInt::uword_t) 1) << (stringLength - 1);
	for (unsigned i = 0; i < stringLength; i++) {
		BigInt value = positive >> i;
		CHECK(value.isTrimmed());
		CHECK(value.toString(2) == positiveString.substr(0, stringLength - i));
		value = negative >> i;
		CHECK(value.isTrimmed());
		CHECK(value.toString(2) == "-1" + std::string(stringLength - i - 1, '0'));
	}
	for (int i = stringLength; i < 1000; i++) {
		BigInt value = positive >> i;
		CHECK(value.isTrimmed());
		CHECK(value.toString() == "0");
		value = negative >> i;
		CHECK(value.isTrimmed());
		CHECK(value.toString() == "-1");
	}
}

TEST_CASE("addition") {
	for (BigInt::sword_t i = -1000; i < 1000; i++) {
		BigInt bigI(i);
		for (BigInt::sword_t j = -100; j < 100; j++) {
			BigInt sum = bigI + BigInt(j);
			CHECK(sum.isTrimmed());
			CHECK(sum.toString() == builtinToString(i + j));
		}
	}

	// Test addition that overflows
	BigInt value((BigInt::uword_t) -2);
	value += BigInt((BigInt::uword_t) 5);
	CHECK(value.isTrimmed());
	CHECK(value.toString(16) == "10000000000000003");

	// Test several-word numbers
	value =  BigInt("0123456789012345678901234567890123456789");
	value += BigInt("9876543210987654321098765432109876543210");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == std::string(40, '9'));
	value =
		BigInt("9876543210987654321098765432109876543210") +
		BigInt("0123456789012345678901234567890123456789");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == std::string(40, '9'));

	// Test edge cases of adding 0-word numbers
	value = BigInt("9876543210987654321098765432109876543210");
	value += BigInt();
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543210");
	value += BigInt((BigInt::sword_t) -1);
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543209");
	value = BigInt();
	value += BigInt("9876543210987654321098765432109876543210");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543210");
	value = BigInt((BigInt::sword_t) -1);
	value += BigInt("9876543210987654321098765432109876543210");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543209");
}

TEST_CASE("subtraction") {
	for (BigInt::sword_t i = -1000; i < 1000; i++) {
		BigInt bigI(i);
		for (BigInt::sword_t j = -100; j < 100; j++) {
			BigInt sum = bigI - BigInt(j);
			CHECK(sum.isTrimmed());
			CHECK(sum.toString() == builtinToString(i - j));
		}
	}

	// Test subtraction that overflows
	BigInt value((BigInt::uword_t) -2);
	value -= BigInt((BigInt::sword_t) -5);
	CHECK(value.isTrimmed());
	CHECK(value.toString(16) == "10000000000000003");

	// Test several-word numbers
	value =
		BigInt("+9876543210987654321098765432109876543210") -
		BigInt("+0123456789012345678901234567890123456789");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9753086421975308642197530864219753086421");
	value =
		BigInt("+9876543210987654321098765432109876543210") -
		BigInt("-0123456789012345678901234567890123456789");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == std::string(40, '9'));
	value =
		BigInt("-9876543210987654321098765432109876543210") -
		BigInt("+0123456789012345678901234567890123456789");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == '-' + std::string(40, '9'));
	value =
		BigInt("-9876543210987654321098765432109876543210") -
		BigInt("-0123456789012345678901234567890123456789");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "-9753086421975308642197530864219753086421");

	// Test edge cases of subtracting 0-word numbers
	value = BigInt("9876543210987654321098765432109876543210");
	value -= BigInt();
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543210");
	value -= BigInt((BigInt::sword_t) -1);
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "9876543210987654321098765432109876543211");
	value = BigInt();
	value -= BigInt("9876543210987654321098765432109876543210");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "-9876543210987654321098765432109876543210");
	value = BigInt((BigInt::sword_t) -1);
	value -= BigInt("9876543210987654321098765432109876543210");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "-9876543210987654321098765432109876543211");
}

TEST_CASE("multiplication") {
	BigInt zero;
	BigInt oneWord((BigInt::uword_t) -1);
	BigInt twoWords = oneWord << 64 | oneWord;
	BigInt value = zero * zero;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");
	value = zero * oneWord;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");
	value = zero * twoWords;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");
	value = oneWord * zero;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");
	value = twoWords * zero;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "0");

	// Test one-word values without carries
	value = BigInt("100") * BigInt("1000");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "100000");
	value = BigInt("1000") * BigInt("100");
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "100000");

	// Test carries
	value = oneWord * oneWord;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "340282366920938463426481119284349108225");
	value = oneWord * twoWords;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "6277101735386680763495507056286727952620534092958556749825");
	value = twoWords * oneWord;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "6277101735386680763495507056286727952620534092958556749825");
	value = twoWords * twoWords;
	CHECK(value.isTrimmed());
	CHECK(value.toString() == "115792089237316195423570985008687907852589419931798687112530834793049593217025");

	// Test large random values
	value =
		BigInt("5b60628f044215620838e243cb52bd4cbd3a73c0ba652a71f9007050090e2035a48bc5cb47c60b1ec450cc7837113de6abff176fa2ddb0bb1513f243c4774a7f2d82c615241b46bdccc522b67c1b6b223244f1e6ec5794678b3804e411b4251ede157aaf", 16) *
		BigInt("64e8ab0c4624185f2dd71d255e493e913a6d7ecfc0f99f64efde869c05c2e43f2f2dbf86f6c0674fdc19c54a59ab369cd44b0f67fd45aa202ce8b00581ecdf8241947d9b99248350977196756064f1100abcaa956bf6d15e7f0ac9d62122040c0ed53735", 16);
	CHECK(value.isTrimmed());
	CHECK(value.toString(16) == "2404B2E6F2A90FFDFD14884FACD67B5B28A6F3CFF5CFAA965BBA67BDBA4A6EFA07B4DF3E47448DCC6DF5FD7FAAD6BE34E386825BA7FB3686F9ADD93B1D7127C8AA9588F3D54D39BACBB0DAF7315DDF60BC8D05E39D7D87162C4840DAF52C49EFAAF0C014D0D9F59619DD173930B5E4DA9598BD69D26896738F344AF0931BEB1301F2593E9234332B82055FFCF745B1F220172657A673156594DF33483E6DDA47270AF4EF10C680F09E6C65DCC93BD4AA580C14FFEB3677177561C009133F09CF8A43C1E33D68FF3B");

	// Test multiplication with negative numbers
	for (BigInt::sword_t i = -1000; i < 1000; i++) {
		BigInt bigI(i);
		for (BigInt::sword_t j = -100; j < 100; j++) {
			BigInt product = bigI * BigInt(j);
			CHECK(product.isTrimmed());
			CHECK(product.toString() == builtinToString(i * j));
		}
	}
}

TEST_CASE("exponentiation") {
	// Test that raising to the power 0 gives 1
	// and raising to the power 1 gives the original number
	for (BigInt::sword_t i = -1000; i < 1000; i++) {
		BigInt bigI(i);
		BigInt power = bigI.pow(0);
		CHECK(power.isTrimmed());
		CHECK(power.toString() == "1");
		power = bigI.pow(1);
		CHECK(power.isTrimmed());
		CHECK(power.toString() == bigI.toString());
	}

	// Test powers of 2
	for (uint8_t i = 0; i < sizeof(BigInt::uword_t) * 8 - 1; i++) {
		BigInt power = BigInt((BigInt::uword_t) 2).pow(i);
		CHECK(power.isTrimmed());
		CHECK(power.toString() == builtinToString((BigInt::uword_t) 1 << i));
	}

	// Test negative numbers
	BigInt::sword_t result = 1;
	for (uint8_t i = 0; i <= 10; i++) {
		BigInt power = BigInt((BigInt::sword_t) -3).pow(i);
		CHECK(power.isTrimmed());
		CHECK(power.toString() == builtinToString(result));
		result *= -3;
	}

	// Test large values
	BigInt large("8FE10177DF321B682B8F2E0DFE5A24833B1801CF6F4735CC611D9710DB712835ACAD5A3F155D915AB8F264678F276F28473430DD752B9EB4BA3FD919A1B48FB3645443185C78EBD916B40511B49E854C86910A7F45FD497B6B8CAD54352230A9B2574C24", 16);
	large = large.pow(5);
	CHECK(large.isTrimmed());
	CHECK(large.toString(16) == "E5B12ED73B341C7A255FED41C3CBB8A6AF81449F10D785BE83D5605B01091095555AB6030F1DFE85DFDE2BC8C60261708EDB5820F18391F5AEB3ED174743245C5E902DCFE4F59A0D896D239345AF224FDF5310F00C16ACC7B71EA62CB59593431CDE56884D7A5467BD2929D67792359BE889D344DD0E211E4CB91C9C1B13F1465BCCC0E2F533C28CCE8B151A1A5195AFDD4141EFE77D4A46CB3901D0EABDCA9FA96030346DEA012982FF639A1AAB7F77B642164B8D6EF15EFC690B05EB850889DE296631DD6AF51CC3F1BE51D242663EA029D3478B2C659724C76D5DF57A3C0A8BFE3158CE4A0CF80585C3BFD13DC14D9A1CDA6290E02A866A6F508ADCE2B20CC1DFC0C816C2E4BB676B3F8B7863AF1FAB9BC9382F0D2F5ED4B10D8D9B12887A2510EBE4984775A966A8B1BCAF611F33BAD2B37B69E1C15C8FFC4847D3607DBA19B6275CC69354F16B4FC1A068D0F31758E0630ACEA7418F86CECFB5E36F0BF265BC244FF912C128B1F65BA1609C81E8FD39D1AAA137C5977C29A99C992EAB88B87F82A70AFA78AC37F842B4F72C2028C97E8E5343BCC4B472975535AFACE02CFD1A8DEF7F17B8D182F8FEB70B46656B4E562541191736DD93AF5FA1283573FD54D20BBBA9FCEF15E0F86175B79A18B436135DDC3AB09B111962319903D61492FCF65F8BA2D56E696931136148F0048C996A400");
}

TEST_CASE("comparison") {
	// Test small values
	for (BigInt::sword_t i = -1000; i < 1000; i++) {
		BigInt bigI(i);
		for (BigInt::sword_t j = -100; j < 100; j++) {
			CHECK(bigI.cmp(BigInt(j)) == signum(i - j));
		}
	}

	// Test comparison operators
	CHECK(!(BigInt("1") == BigInt("0")));
	CHECK(BigInt("1") == BigInt("1"));
	CHECK(!(BigInt("1") == BigInt("2")));

	CHECK(BigInt("1") != BigInt("0"));
	CHECK(!(BigInt("1") != BigInt("1")));
	CHECK(BigInt("1") != BigInt("2"));

	CHECK(!(BigInt("1") < BigInt("0")));
	CHECK(!(BigInt("1") < BigInt("1")));
	CHECK(BigInt("1") < BigInt("2"));

	CHECK(!(BigInt("1") <= BigInt("0")));
	CHECK(BigInt("1") <= BigInt("1"));
	CHECK(BigInt("1") <= BigInt("2"));

	CHECK(BigInt("1") > BigInt("0"));
	CHECK(!(BigInt("1") > BigInt("1")));
	CHECK(!(BigInt("1") > BigInt("2")));

	CHECK(BigInt("1") >= BigInt("0"));
	CHECK(BigInt("1") >= BigInt("1"));
	CHECK(!(BigInt("1") >= BigInt("2")));

	// Test multi-word numbers
	CHECK(
		BigInt("100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("99999999999999999999999999999999999999999999999999")) == +1
	);
	CHECK(
		BigInt("100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("100000000000000000000000000000000000000000000000000")) == 0
	);
	CHECK(
		BigInt("100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("100000000000000000000000000000000000000000000000001")) == -1
	);

	CHECK(
		BigInt("-100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("-99999999999999999999999999999999999999999999999999")) == -1
	);
	CHECK(
		BigInt("-100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("-100000000000000000000000000000000000000000000000000")) == 0
	);
	CHECK(
		BigInt("-100000000000000000000000000000000000000000000000000")
		.cmp(BigInt("-100000000000000000000000000000000000000000000000001")) == +1
	);
}
