# Sets up the build type.
function (fortuno_setup_build_type default_build_type)

  get_property(_multiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if(_multiConfig)
    message(STATUS "Build type multi-config (build type selected at the build step)")
  else()
    if(NOT CMAKE_BUILD_TYPE)
      message(STATUS "Build type ${default_build_type} (default single-config)")
      set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Build type" FORCE)
      set_property(CACHE CMAKE_BUILD_TYPE PROPERTY HELPSTRING "Choose the type of build")
      set_property(
        CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo"
      )
    else()
      message(STATUS "Fortuno: build type: ${CMAKE_BUILD_TYPE} (manually selected single-config)")
    endif()
  endif()

endfunction()
