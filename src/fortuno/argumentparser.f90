! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a simple command line argument parser.
module fortuno_argumentparser
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : basename, nl, string, string_list
  implicit none

  type :: argument_types_enum_
    integer :: bool = 1
    integer :: int = 2
    integer :: float = 3
    integer :: string = 4
    integer :: stringlist = 5
  end type argument_types_enum_

  type(argument_types_enum_), parameter :: argtypes = argument_types_enum_()


  !> Contains a definition of a command line argument
  type :: argument_def

    !> name associated with the argument
    character(:), allocatable :: name

    !> type of the argument (one field of argtypes)
    integer :: argtype = -1

    !> short option form, use "" for options with long form only and for positional arguments
    character :: shortopt = ""

    !> long option form, leave unallocated for options with short form only and positional arguments
    character(:), allocatable :: longopt

    !> default value to use, if option had not been specified, leave unallocated for no default
    class(*), allocatable :: default

    !> help message to print for the argument
    character(:), allocatable :: helpmsg

  end type argument_def


  !> An argument value obtained after parsing
  type :: argument_value

    !> Name associated with the argument
    character(:), allocatable :: name

    !> Parsed value of the argument (unallocated for logical arguments)
    class(*), allocatable :: argval

  end type argument_value


  !> Collection of all argument values obtained after command line had been prased
  type :: argument_values
    private
    type(argument_value), allocatable :: argvals(:)
  contains
    procedure :: has => argument_values_has
    procedure :: get_value_stringlist => argument_values_get_value_stringlist
    generic :: get_value => get_value_stringlist
  end type argument_values


  !> Argument parser.
  type :: argument_parser
    private
    type(argument_def), allocatable :: argdefs(:)
    character(:), allocatable :: description
  contains
    procedure :: parse_args => argument_parser_parse_args
  end type argument_parser


  integer, parameter :: terminal_width_ = 80

contains

  !> Initializes an argument parser.
  subroutine init_argument_parser(this, description, argdefs)

    !> instance
    type(argument_parser), intent(out) :: this

    !> description to print when help is required
    character(*), intent(in) :: description

    !> argument definitions
    type(argument_def), intent(in) :: argdefs(:)

    this%argdefs = argdefs
    this%description = description

  end subroutine init_argument_parser


  !> Parses command command line arguments.
  subroutine argument_parser_parse_args(this, argumentvalues, logger, exitcode)

    !> instance
    class(argument_parser), intent(inout) :: this

    !> argument values obtained during parsing
    type(argument_values), intent(out) :: argumentvalues

    !> logger for issuing messages
    class(test_logger), intent(inout) :: logger

    !> Exit code (-1, if processing can continue, >= 0 if processing should stop)
    integer, intent(out) :: exitcode


    type(string), allocatable :: cmdargs(:), posargs(:)
    logical, allocatable :: processed(:)
    character(:), allocatable :: argname, errormsg
    integer :: nargs, nargdefs, iarg, iargdef
    logical optionsallowed, islong, matches

    exitcode = -1

    call get_command_line_args_(cmdargs)
    nargs = ubound(cmdargs, dim=1)
    nargdefs = size(this%argdefs)
    allocate(processed(nargdefs), source=.false.)

    allocate(argumentvalues%argvals(0))
    allocate(posargs(0))
    optionsallowed = .true.
    iarg = 0
    argloop: do while (iarg < nargs)
      iarg = iarg + 1
      associate (arg => cmdargs(iarg)%content)
        if (arg == "--") then
          optionsallowed = .false.
          cycle
        end if
        if (.not. optionsallowed .or. arg(1:1) /= "-") then
          posargs =  [posargs, string(arg)]
          cycle
        end if
        islong = arg(1:min(len(arg), 2)) == "--"
        if (islong) then
          argname = arg(3:)
        else if (len(arg) == 2) then
          argname = arg(2:2)
        else
          call logger%log_error("Invalid short option '" // cmdargs(iarg)%content // "'")
          exitcode = 1
          return
        end if
        if ((islong .and. argname == "help") .or. (.not. islong .and.  argname == "h")) then
          call print_help_(logger, cmdargs(0)%content, this%description, this%argdefs)
          exitcode = 0
          return
        end if
        do iargdef = 1, nargdefs
          associate (argdef => this%argdefs(iargdef))
            matches = .false.
            if (islong .and. allocated(argdef%longopt)) then
              matches = argdef%longopt == argname
            else if (.not. islong .and. argdef%shortopt /= "") then
              matches = argdef%shortopt == argname
            end if
            if (matches) then
              select case (argdef%argtype)
              case (argtypes%bool)
                argumentvalues%argvals = [argumentvalues%argvals, argument_value(name=argdef%name)]
              case default
                call logger%log_error("Unknown argument type")
                exitcode = 1
                return
              end select
              cycle argloop
            end if
          end associate
        end do
        call logger%log_error("invalid option '" // arg // "'")
        exitcode = 1
        return
      end associate
    end do argloop
    associate (argdef => this%argdefs(nargdefs))
      if (.not. allocated(argdef%longopt) .and. argdef%shortopt == "") then
        argumentvalues%argvals = [argumentvalues%argvals,&
            & argument_value(name=argdef%name, argval=string_list(posargs))]
      else if (size(posargs) > 1) then
        call logger%log_error("Superfluous positional arguments found")
        exitcode = 1
        return
      end if
    end associate

  end subroutine argument_parser_parse_args


  !> Cheks whether a given name is contained in the argument value collection.
  function argument_values_has(this, name) result(found)

    !> instance
    class(argument_values), intent(in) :: this

    !> name to search for
    character(*), intent(in) :: name

    !> Whether collection has entry with given name
    logical :: found

    integer :: iargval

    found = .false.
    do iargval = 1, size(this%argvals)
      found = this%argvals(iargval)%name == name
      if (found) return
    end do

  end function argument_values_has


  !> Returns the value of a parsed argument as array of strings.
  subroutine argument_values_get_value_stringlist(this, name, val)

    !> instance
    class(argument_values), intent(in) :: this

    !> name of the argument
    character(*), intent(in) :: name

    !> value on exit
    type(string), allocatable, intent(out) :: val(:)

    logical :: found
    integer :: iargval

    found = .false.
    do iargval = 1, size(this%argvals)
      found = this%argvals(iargval)%name == name
      if (found) exit
    end do
    if (found) then
      select type (argval => this%argvals(iargval)%argval)
      type is (string_list)
        val = argval%items
      class default
        error stop "Invalid argument type for argument '" // name // "'"
      end select
    else
      error stop "Argument '" // name // "' not found"
    end if

  end subroutine argument_values_get_value_stringlist


  !! Returns the command line arguments as an array of strings.
  subroutine get_command_line_args_(cmdargs)
    type(string), allocatable :: cmdargs(:)

    integer :: nargs, iarg, arglen

    nargs = command_argument_count()
    allocate(cmdargs(0:nargs))
    do iarg = 0, nargs
      call get_command_argument(iarg, length=arglen)
      allocate(character(arglen) :: cmdargs(iarg)%content)
      call get_command_argument(iarg, value=cmdargs(iarg)%content)
    end do

  end subroutine get_command_line_args_


  !! Prints help information.
  subroutine print_help_(logger, scriptname, description, argdefs)
    class(test_logger), intent(inout) :: logger
    character(*), intent(in) :: scriptname, description
    type(argument_def), intent(in) :: argdefs(:)

    integer :: iargdef
    character(:), allocatable :: line, buffer

    line = "Usage: " // basename(scriptname) // " [-h]"
    do iargdef = 1, size(argdefs)
      associate(argdef => argdefs(iargdef))
        if (argdef%shortopt /= "") then
          line = line // " [-" // argdef%shortopt // "]"
        else if (allocated(argdef%longopt)) then
          line = line // " [--" // argdef%longopt // "]"
        else
          line = line // " [" // argdef%name // "]"
        end if
      end associate
    end do
    call logger%log_message(line // nl // nl)
    call logger%log_message(description)
    associate (argdef => argdefs(size(argdefs)))
      ! If last argument is a positional argument
      if (argdef%shortopt == "" .and. .not. allocated(argdef%longopt)) then
        call print_argument_help_(logger, argdef%name, argdef%helpmsg, terminal_width_)
      end if
    end associate
    call logger%log_message(nl // "Options:")
    call print_argument_help_(logger, "-h, --help", "show this help message and exit",&
        & terminal_width_)
    do iargdef = 1, size(argdefs)
      associate(argdef => argdefs(iargdef))
        if (argdef%shortopt /= "" .and. allocated(argdef%longopt)) then
          buffer = "-" // argdef%shortopt // ", --" // argdef%longopt
        else if (argdef%shortopt /= "") then
          buffer = "-" // argdef%shortopt
        else if (allocated(argdef%longopt)) then
          buffer = "--" // argdef%longopt
        else
          cycle
        end if
        line = "  " // buffer // repeat(" ", max(0, 24 - len(buffer) - 2)) // argdef%helpmsg
        call logger%log_message(line)
      end associate
    end do

  end subroutine print_help_


  !! Prints the help for a single argument.
  subroutine print_argument_help_(logger, argument, helpmsg, linelength)
    class(test_logger), intent(inout) :: logger
    character(*), intent(in) :: argument, helpmsg
    integer, intent(in) :: linelength

    integer, parameter :: offset = 25
    character(20) :: formatstr
    character(linelength) :: buffer
    integer :: maxwidth, curpos, seppos

    write(formatstr, "(a, i0, a)") "(2x, a, t", offset, ", a)"
    maxwidth = linelength - offset
    curpos = 1
    do while (curpos <= len(helpmsg))
      if (curpos + maxwidth - 1 > len(helpmsg)) then
        if (curpos == 1) then
          write(buffer, formatstr) argument, helpmsg(curpos:)
        else
          write(buffer, formatstr) "", helpmsg(curpos:)
        end if
        call logger%log_message(trim(buffer))
        exit
      else
        seppos = index(helpmsg(curpos : curpos + maxwidth), " ", back=.true.) + curpos - 1
        if (seppos < curpos + maxwidth / 2) seppos = curpos + maxwidth
        if (curpos == 1) then
          write(buffer, formatstr) argument, helpmsg(curpos : seppos - 1)
        else
          write(buffer, formatstr) "", helpmsg(curpos : seppos - 1)
        end if
        call logger%log_message(buffer)
        if (helpmsg(seppos:seppos) == " ") then
          curpos = seppos + 1
        else
          curpos = seppos
        end if
      end if
    end do

  end subroutine print_argument_help_

end module fortuno_argumentparser