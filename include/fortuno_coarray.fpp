! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#ifndef __FORTUNO_COARRAY_FPP__
#define __FORTUNO_COARRAY_FPP__

#define CHECK(ctx, cond)\
  block;\
    call ctx%check(cond, file=__FILE__, line=__LINE__);\
  end block;

#define CHECK_MSG(ctx, cond, msg)\
  block;\
    call ctx%check(cond, msg, file=__FILE__, line=__LINE__);\
  end block;

#define ASSERT(ctx, cond)\
  block;\
    call ctx%check(cond, file=__FILE__, line=__LINE__);\
    if (ctx%check_failed()) return;\
  end block;

#define ASSERT_MSG(ctx, cond, msg)\
  block;\
    call ctx%check(cond, msg, file=__FILE__, line=__LINE__);\
    if (ctx%check_failed()) return;\
  end block;

#endif
