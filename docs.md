A type is concrete if:
	It has the TRIVIAL flag.
	Or:
		It is not generic.
		All fields are concrete.

For every type in a module
	If the type is concrete: Do nothing.
	Otherwise:
		Create `copy`, `move` and `drop` functions.

		If the type is generic:
			Create `mk_generics`, `mk_vwt` and `mk_info` functions.

When a type's info is requested:
	If the type is concrete:
		If the type is 0, 1, 2, 4, 8 or 16 bytes: Use one of the TRIVIAL_TIS.
		Otherwise:
			Create a new type info + vwt for that size using the trivial `copy`, `move` and `drop` functions.
			This info should be memoized.
	Otherwise:
		If the type is generic:
			Use the `mk_generics`, `mk_vwt` and `mk_info` functions of that type.
		Otherwise:
			Create a new type info + vwt using the `copy`, `move` and `drop` functions of that type.
			This info should be memoized.