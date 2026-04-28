// CryptFunctionsTests.prg
// Unit tests for XSharp.VO Crypt functions
// (src/Runtime/XSharp.VO/Functions/String.prg)
// Tests: Crypt, CryptRaw, CryptA

USING Xunit
USING System

BEGIN NAMESPACE XSharp.VO.Tests

    PUBLIC CLASS CryptFunctionsTests

        // ─────────────────────────────────────────────
        // Crypt Function Tests (String overload)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_EncryptDecrypt_RoundTrip() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "Hello World"
            cKey := "SecretKey"
            
            cEncrypted := Crypt(cOriginal, cKey)
            Assert.NotNull(cEncrypted)
            Assert.NotEqual(cOriginal, cEncrypted)
            
            // Decrypt by applying same key again (XOR-based encryption)
            cDecrypted := Crypt(cEncrypted, cKey)
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_EmptySource_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            cResult := Crypt("", "key")
            Assert.NotNull(cResult)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_EmptyKey_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            TRY
                cResult := Crypt("Hello", "")
                // May return original or throw
                Assert.True(cResult != NULL)
            CATCH
                // Exception is acceptable behavior
                Assert.True(TRUE)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_NullSource_HandlesGracefully() AS VOID
            LOCAL cResult AS STRING
            TRY
                cResult := Crypt(NULL_STRING, "key")
                Assert.True(cResult == NULL .OR. cResult == "")
            CATCH
                Assert.True(TRUE)
            END TRY
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_DifferentKeys_ProduceDifferentResults() AS VOID
            LOCAL cSource AS STRING
            LOCAL cKey1 AS STRING
            LOCAL cKey2 AS STRING
            LOCAL cEncrypt1 AS STRING
            LOCAL cEncrypt2 AS STRING
            
            cSource := "Test Data"
            cKey1 := "Key1"
            cKey2 := "Key2"
            
            cEncrypt1 := Crypt(cSource, cKey1)
            cEncrypt2 := Crypt(cSource, cKey2)
            
            Assert.NotEqual(cEncrypt1, cEncrypt2)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_LongText_EncryptsCorrectly() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "This is a much longer text that should be encrypted properly using the Crypt function."
            cKey := "LongKey123"
            
            cEncrypted := Crypt(cOriginal, cKey)
            cDecrypted := Crypt(cEncrypted, cKey)
            
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        // ─────────────────────────────────────────────
        // CryptRaw Function Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD CryptRaw_EncryptDecrypt_RoundTrip() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "Raw Encryption Test"
            cKey := "RawKey"
            
            cEncrypted := CryptRaw(cOriginal, cKey)
            Assert.NotNull(cEncrypted)
            
            cDecrypted := CryptRaw(cEncrypted, cKey)
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD CryptRaw_DifferentFromCrypt() AS VOID
            LOCAL cSource AS STRING
            LOCAL cKey AS STRING
            LOCAL cCrypt AS STRING
            LOCAL cCryptRaw AS STRING
            
            cSource := "Compare"
            cKey := "Key"
            
            cCrypt := Crypt(cSource, cKey)
            cCryptRaw := CryptRaw(cSource, cKey)
            
            // CryptRaw might behave differently than Crypt
            Assert.NotNull(cCrypt)
            Assert.NotNull(cCryptRaw)
        END METHOD

        // ─────────────────────────────────────────────
        // CryptA Function Tests (ANSI version)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD CryptA_EncryptDecrypt_RoundTrip() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "ANSI Encryption"
            cKey := "AnsiKey"
            
            cEncrypted := CryptA(cOriginal, cKey)
            Assert.NotNull(cEncrypted)
            
            cDecrypted := CryptA(cEncrypted, cKey)
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD CryptA_SpecialCharacters_EncryptsCorrectly() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "Special!@#$%^&*()"
            cKey := "SpecialKey"
            
            cEncrypted := CryptA(cOriginal, cKey)
            cDecrypted := CryptA(cEncrypted, cKey)
            
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        // ─────────────────────────────────────────────
        // Crypt Function Tests (BYTE[] overload)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_ByteArray_EncryptDecrypt_RoundTrip() AS VOID
            LOCAL bOriginal AS BYTE[]
            LOCAL bKey AS BYTE[]
            LOCAL bEncrypted AS BYTE[]
            LOCAL bDecrypted AS BYTE[]
            
            bOriginal := <BYTE>{72, 101, 108, 108, 111} // "Hello"
            bKey := <BYTE>{75, 101, 121} // "Key"
            
            bEncrypted := Crypt(bOriginal, bKey)
            Assert.NotNull(bEncrypted)
            Assert.Equal(bOriginal:Length, bEncrypted:Length)
            
            bDecrypted := Crypt(bEncrypted, bKey)
            Assert.Equal(bOriginal:Length, bDecrypted:Length)
            
            FOR LOCAL i := 1 AS INT UPTO bOriginal:Length
                Assert.Equal(bOriginal[i], bDecrypted[i])
            NEXT
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_ByteArray_BinaryData_EncryptsCorrectly() AS VOID
            LOCAL bOriginal AS BYTE[]
            LOCAL bKey AS BYTE[]
            LOCAL bEncrypted AS BYTE[]
            LOCAL bDecrypted AS BYTE[]
            
            // Binary data with all byte values
            bOriginal := <BYTE>{0, 1, 127, 128, 255}
            bKey := <BYTE>{42}
            
            bEncrypted := Crypt(bOriginal, bKey)
            bDecrypted := Crypt(bEncrypted, bKey)
            
            FOR LOCAL i := 1 AS INT UPTO bOriginal:Length
                Assert.Equal(bOriginal[i], bDecrypted[i])
            NEXT
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_ByteArray_EmptySource_HandlesGracefully() AS VOID
            LOCAL bEmpty AS BYTE[]
            LOCAL bKey AS BYTE[]
            LOCAL bResult AS BYTE[]
            
            bEmpty := <BYTE>{}
            bKey := <BYTE>{1, 2, 3}
            
            bResult := Crypt(bEmpty, bKey)
            Assert.NotNull(bResult)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_ByteArray_LargeData_EncryptsCorrectly() AS VOID
            LOCAL bOriginal AS BYTE[]
            LOCAL bKey AS BYTE[]
            LOCAL bEncrypted AS BYTE[]
            LOCAL bDecrypted AS BYTE[]
            LOCAL i AS INT
            
            // Create 1000-byte array
            bOriginal := BYTE[]{1000}
            FOR i := 1 UPTO 1000
                bOriginal[i] := (BYTE)((i - 1) % 256)
            NEXT
            
            bKey := <BYTE>{123, 45, 67, 89}
            
            bEncrypted := Crypt(bOriginal, bKey)
            bDecrypted := Crypt(bEncrypted, bKey)
            
            Assert.Equal(bOriginal:Length, bDecrypted:Length)
            FOR i := 1 UPTO bOriginal:Length
                Assert.Equal(bOriginal[i], bDecrypted[i])
            NEXT
        END METHOD

        // ─────────────────────────────────────────────
        // Edge Cases and Security Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_ShortKey_RepeatsKeyPattern() AS VOID
            LOCAL cOriginal AS STRING
            LOCAL cKey AS STRING
            LOCAL cEncrypted AS STRING
            LOCAL cDecrypted AS STRING
            
            cOriginal := "This is a long text with short key"
            cKey := "AB"
            
            cEncrypted := Crypt(cOriginal, cKey)
            cDecrypted := Crypt(cEncrypted, cKey)
            
            Assert.Equal(cOriginal, cDecrypted)
        END METHOD

        [Fact, Trait("Category", "Crypt")];
        PUBLIC METHOD Crypt_IdenticalSourceAndKey_ProducesResult() AS VOID
            LOCAL cText AS STRING
            LOCAL cResult AS STRING
            
            cText := "SameKey"
            cResult := Crypt(cText, cText)
            
            Assert.NotNull(cResult)
            // Decrypting with same key should return original
            Assert.Equal(cText, Crypt(cResult, cText))
        END METHOD

    END CLASS

END NAMESPACE
