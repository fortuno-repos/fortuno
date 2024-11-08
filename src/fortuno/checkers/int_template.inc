
  !> Checks whether two integer values are equal
  function is_equal_r0_r0(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1

    !> Second value to check
    integer(ik), intent(in) :: value2

    !> Result of the check
    type(check_result) :: checkresult

    logical :: match

    match = value1 == value2
    if (.not. match) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1), value_item(value2), checkresult)
      return
    end if
    checkresult%success = .true.

  end function is_equal_r0_r0


  !> Checks whether two integer rank 1 arrays are equal.
  function all_equal_r1_r1(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1(:)

    !> Second value to check
    integer(ik), intent(in) :: value2(:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    if (any(shape(value1, kind=i64) /= shape(value2, kind=i64))) then
      call add_shape_mismatch_details(shape(value1, kind=i64), shape(value2, kind=i64),&
          & checkresult)
      return
    end if
    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1(mismatchloc(1))), value_item(value2(mismatchloc(1))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r1_r1


  !> Checks whether two integer rank 1 arrays are equal.
  function all_equal_r2_r2(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1(:,:)

    !> Second value to check
    integer(ik), intent(in) :: value2(:,:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    if (any(shape(value1, kind=i64) /= shape(value2, kind=i64))) then
      call add_shape_mismatch_details(shape(value1, kind=i64), shape(value2, kind=i64),&
          & checkresult)
      return
    end if
    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1(mismatchloc(1), mismatchloc(2))),&
          & value_item(value2(mismatchloc(1), mismatchloc(2))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r2_r2


  !> Checks whether an integer rank 1 arrays is equal to a scalar.
  function all_equal_r1_r0(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1(:)

    !> Second value to check
    integer(ik), intent(in) :: value2

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1(mismatchloc(1))), value_item(value2),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r1_r0


  !> Checks whether a scalar integer is equal to an integer rank 1 array.
  function all_equal_r0_r1(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1

    !> Second value to check
    integer(ik), intent(in) :: value2(:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:)
    integer(i64) :: mismatchloc(1)

    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1), value_item(value2(mismatchloc(1))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r0_r1


  !> Checks whether two integer rank 1 arrays are equal.
  function all_equal_r2_r0(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1(:,:)

    !> Second value to check
    integer(ik), intent(in) :: value2

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1(mismatchloc(1), mismatchloc(2))),&
          & value_item(value2), checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r2_r0


  !> Checks whether two integer rank 1 arrays are equal.
  function all_equal_r0_r2(value1, value2) result(checkresult)

    !> First value to check
    integer(ik), intent(in) :: value1

    !> Second value to check
    integer(ik), intent(in) :: value2(:,:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: match(:,:)
    integer(i64) :: mismatchloc(2)

    match = value1 == value2
    mismatchloc = findloc(match, .false., kind=i64)
    if (all(mismatchloc /= 0)) then
      call add_value_mismatch_details("mismatching integer values",&
          & value_item(value1),&
          & value_item(value2(mismatchloc(1), mismatchloc(2))),&
          & checkresult, mismatchloc=mismatchloc)
      return
    end if
    checkresult%success = .true.

  end function all_equal_r0_r2
