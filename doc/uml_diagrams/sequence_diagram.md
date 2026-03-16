```mermaid
sequenceDiagram
    participant objectfoo
    participant sp_smart_pointer_t 
    participant new_foofoo
    participant cpp_object

    sp_smart_pointer_t ->>objectfoo: 

    objectfoo->>+new_foofoo: foo()
    new_foofoo->>cpp_object: cpp new foo()
    cpp_object->>new_foofoo: id
    

    new_foofoo->>new_foofoo: register_self()
    new_foofoo->>sp_smart_pointer_t: sp_smart_pointer_t()
    
    sp_smart_pointer_t->>countint: <<create>> 
    

    sp_smart_pointer_t->>sp_resource_t: <<create>>
    
    sp_smart_pointer_t->>sp_smart_pointer_t: grab(){count=1} 
    sp_smart_pointer_t-->>new_foofoo: complete sp_smart_pointer_t()

    sp_smart_pointer_t->>sp_smart_pointer_t: sp_smart_pointer_t::assign()
    sp_smart_pointer_t->>sp_smart_pointer_t: link count & obj
    sp_smart_pointer_t->>sp_smart_pointer_t: grab() {count=2} 
    sp_smart_pointer_t-->>sp_smart_pointer_t: complete sp_smart_pointer_t::assign()
    sp_smart_pointer_t-->>sp_smart_pointer_t: finalize sp_smart_pointer_t

    sp_smart_pointer_t-->>new_foofoo: complete register_self()

    new_foofoo-->>objectfoo: complete foo()
    objectfoo->>new_foofoo: assignment

    sp_smart_pointer_t->>sp_smart_pointer_t: ref_counter::assign()
    sp_smart_pointer_t->>sp_smart_pointer_t: link count & obj
    sp_smart_pointer_t->>sp_smart_pointer_t: grab() {count=2}
    sp_smart_pointer_t-->>sp_smart_pointer_t: complete sp_smart_pointer_t::assign()
    new_foofoo-->>objectfoo: complete assignment
    new_foofoo-->>sp_smart_pointer_t: finalize sp_smart_pointer_t
    sp_smart_pointer_t-->>sp_smart_pointer_t: release() {count
