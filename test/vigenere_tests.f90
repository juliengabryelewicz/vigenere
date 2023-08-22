module vigenere_tests
  use vigenere, only: vigenere_crypt, vigenere_translate

  implicit none

  private
  public :: test_vigenere_crypt, test_vigenere_translate

contains

  subroutine test_vigenere_crypt

    character(:), allocatable :: result, expect, sentence, key
    expect = "XMIMRWKYIBKWG"
    sentence = "CECI EST UN TEST"
    key = "VIGENERE"
    result = vigenere_crypt(sentence, key)

    print *,result

    if (result == expect) then
      print *, 'test crypt passed'
    else
      print *, 'test crypt failed'
    end if

  end subroutine test_vigenere_crypt

  subroutine test_vigenere_translate

    character(:), allocatable :: result, expect, sentence, key
    sentence = "XMIMRWKYIBKWG"
    expect = "CECIESTUNTEST"
    key = "VIGENERE"
    result = vigenere_translate(sentence, key)

    print *,result

    if (result == expect) then
      print *, 'test translate passed'
    else
      print *, 'test translate failed'
    end if

  end subroutine test_vigenere_translate

end module vigenere_tests


program run_tests
  use vigenere_tests, only: test_vigenere_crypt, test_vigenere_translate
  call test_vigenere_crypt()
  call test_vigenere_translate()
end program run_tests