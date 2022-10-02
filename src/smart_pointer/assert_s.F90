submodule(assert_m) assert_s
  implicit none

contains

  module procedure assert

    if (enforce_assertions) then
#ifdef XLF
      if (.not. assertion) error stop 999
#else
      if (.not. assertion) error stop description
#endif
    end if

  end procedure

end submodule assert_s
