Classes
-------

The Unified Modeling Language (UML) class diagram below depicts the classes
in the Smart-Pointer library.  Non-abstract user-defined derived types that
extend `sp_smart_pointer_t` inherit an obligation to define the `free` deferred
binding according to the `free_interface` abstract interface defined in 
`sp_resource_m`.  The user-defined `free` subroutine must free the associated
resource, which usually means deallocating the associated memory.

```mermaid
---
title: Smart Pointers Class Diagram 
---
classDiagram

class sp_resource_t {
    free()
}
    <<Abstract>> sp_resource_t
    sp_resource_t--*sp_reference_counter_t

class sp_reference_counter_t {
    -count_: integer, pointer
    -object_: sp_resource_t
    grab()
    release()   
}
sp_reference_counter_t--*sp_smart_pointer_t

class sp_smart_pointer_t {
    -counter_:counter_t
    release_handle()
    start_counter()
}
<<Abstract>> sp_smart_pointer_t
    sp_smart_pointer_t--|>sp_resource_t

```
