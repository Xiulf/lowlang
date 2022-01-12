For every type in a module
	If the type is trivial:
		Do nothing.
	If the type is generic:
		Create `copy`, `move` and `drop` functions.
		Create `mk_generics`, `mk_vwt` and `mk_info` functions.
	Otherwise:
		Create `copy`, `move` and `drop` functions.
		Create a new type info and vwt for this type.

When a type's info is requested:
	If the type is trivial:
		If the type is 0, 1, 2, 4, 8 or 16 bytes: Use one of the TRIVIAL_TIS.
		Otherwise:
			Create a new type info + vwt for that size using the trivial `copy`, `move` and `drop` functions.
			This info should be memoized.
	Otherwise:
		If the type is generic:
			Use the `mk_generics`, `mk_vwt` and `mk_info` functions of that type.
		Otherwise:
			Lookup the type info