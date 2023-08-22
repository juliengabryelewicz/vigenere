module vigenere
  implicit none
  public :: vigenere_crypt, vigenere_translate
contains


  function vigenere_crypt(sentence,key) result(translated)
    character(:), allocatable :: sentence, translated, key
    character :: ch
    integer :: i,j
    sentence = trim(sentence)
    translated = ""
    j = 1

    do i = 1, len(sentence)
      if (sentence(i:i) /= " ") then
        translated = translated // achar(modulo(iachar(sentence(i:i)) + iachar(key(j:j)), 26) + 65)
        j = j + 1
        if(j > len(key)) then 
          j = j - len(key)
        end if
      end if
    end do

  end function vigenere_crypt

  function vigenere_translate(sentence,key) result(translated)
    character(:), allocatable :: sentence, translated, key
    character :: ch
    integer :: i,j
    sentence = trim(sentence)
    translated = ""
    j = 1
    
    do i = 1, len(sentence)
      translated = translated // achar(modulo(iachar(sentence(i:i)) - iachar(key(j:j)), 26) + 65)
      j = j + 1
      if(j > len(key)) then 
        j = j - len(key)
      end if
    end do

  end function vigenere_translate
end module vigenere
