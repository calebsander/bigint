#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#include "bigint.hpp"

std::string builtinToString(const BigInt::sword_t n) {
	std::ostringstream stream;
	stream << n;
	return stream.str();
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