
  !> Checks whether two real numbers are close to each other.
  function is_close_r0_r0(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1

    !> Second value to check
    real(rk), intent(in) :: value2

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical :: match

    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    if (.not. match) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1), value_item(value2), checkresult)
      return
    end if
    checkresult%success = .true.

  end function is_close_r0_r0


  !> Checks whether all elements of two real rank one arrays are close to each other.
  function all_close_r1_r1(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1(:)

    !> Second value to check
    real(rk), intent(in) :: value2(:)

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    if (any(shape(value1, kind=i64) /= shape(value2, kind=i64))) then
      call add_shape_mismatch_details(shape(value1, kind=i64), shape(value2, kind=i64),&
          & checkresult)
      return
    end if
    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1(mismatchloc(1))), value_item(value2(mismatchloc(1))), checkresult,&
          & mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r1_r1


  !> Checks whether all elements of two real rank one arrays are close to each other.
  function all_close_r2_r2(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1(:,:)

    !> Second value to check
    real(rk), intent(in) :: value2(:,:)

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    if (any(shape(value1, kind=i64) /= shape(value2, kind=i64))) then
      call add_shape_mismatch_details(shape(value1, kind=i64), shape(value2, kind=i64),&
          & checkresult)
      return
    end if
    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1(mismatchloc(1), mismatchloc(2))),&
          & value_item(value2(mismatchloc(1), mismatchloc(2))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r2_r2


  !> Checks whether all elements of a rank1 real array and a scalar are close
  function all_close_r1_r0(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1(:)

    !> Second value to check
    real(rk), intent(in) :: value2

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1(mismatchloc(1))), value_item(value2), checkresult,&
          & mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r1_r0


  !> Checks whether a scalar and all elements of a rank1 real array are close
  function all_close_r0_r1(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1

    !> Second value to check
    real(rk), intent(in) :: value2(:)

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1), value_item(value2(mismatchloc(1))), checkresult,&
          & mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r0_r1


  !> Checks whether all elements of a rank2 real array and a scalar are close
  function all_close_r2_r0(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1(:,:)

    !> Second value to check
    real(rk), intent(in) :: value2

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1(mismatchloc(1), mismatchloc(2))),&
          & value_item(value2),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r2_r0


  !> Checks whether a scalar and all elements of a rank2 real array are close
  function all_close_r0_r2(value1, value2, rtol, atol) result(checkresult)

    !> First value to check
    real(rk), intent(in) :: value1

    !> Second value to check
    real(rk), intent(in) :: value2(:,:)

    !> Relative tolerance for the comparison
    real(rk), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison
    real(rk), optional, intent(in) :: atol

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    match = is_close_elem(value1, value2, atol=atol, rtol=rtol)
    mismatchloc(:) = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("real values differing beyond tolerance",&
          & value_item(value1),&
          & value_item(value2(mismatchloc(1), mismatchloc(2))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_close_r0_r2
