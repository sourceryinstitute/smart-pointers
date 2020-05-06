add_executable(test-count-leaks
  test-count-leaks.f90
  faux_cpp_server.f90
  vector.f90
)
target_link_libraries(test-count-leaks
  PRIVATE refcount
)
target_include_directories(refcount
  PUBLIC $<TARGET_PROPERTY:refcount,Fortran_MODULE_DIRECTORY>
)
