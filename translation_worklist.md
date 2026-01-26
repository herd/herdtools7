# Typing Relations/Functions

## AssignableExpressions.tex
1. annotate_lexpr // completed
2. check_disjoint_slices // completed
3. fold_bitvector_fields // completed

## BaseValues.tex
4. base_value // completed
5. constraint_abs_min // completed
6. list_min_abs // completed

## Bitfields.tex
7. absolute_bitfields_align // completed
8. annotate_bitfield // completed
9. annotate_bitfields // completed
10. bitfield_get_name // completed
11. bitfield_get_nested // completed
12. bitfield_get_slices // completed
13. bitfield_slice_to_positions // completed
14. bitfield_to_absolute // completed
15. bitfields_to_absolute // completed
16. check_common_bitfields_align // completed
17. check_positions_in_width // completed
18. check_slices_in_width // completed
19. disjoint_slices_to_positions // completed
20. slice_to_indices // completed

## BlockStatements.tex
21. annotate_block // completed

## Expressions.tex
22. annotate_expr // completed
23. annotate_field_init // completed
24. annotate_get_array // completed
25. check_atc // completed
26. find_bitfields_slices // completed
27. get_bitfield_width // completed
28. width_plus // completed

## Literals.tex
29. annotate_literal // completed

## PrimitiveOperations.tex
30. unop_literals // completed
31. binary_to_unsigned // completed
32. binop_literals // completed

## Slicing.tex
33. annotate_slice // completed
34. annotate_slices // completed
35. annotate_symbolic_constrained_integer // completed
36. slice_width // completed
37. slices_width // completed

## Statements.tex
38. annotate_stmt // completed
39. annotate_limit_expr // completed (January 29)
40. annotate_local_decl_type_annot // completed (January 29)
    BUGFIX: in `annotate_local_decl_type_annot`, the argument `e'` was missing side effects.
    BUGIX: `annotate_type` was missing the first (FALSE) argument.
41. check_can_be_initialized_with // completed (January 29)
42. check_no_precision_loss // completed (January 29)
43. get_for_constraints // completed (January 29)
44. inherit_integer_constraints // completed (January 29)

## TypeAttributes.tex
45. check_constrained_integer // completed
46. get_structure // completed
47. is_anonymous // completed
48. is_builtin_singular // completed
49. is_named // completed
50. is_singular // completed
51. is_structured // completed
52. make_anonymous // completed

## TypeSystemUtilities.tex
53. lookup_constant // completed
54. unique_list // completed (January 28)
55. unique_list_acc // completed (January 28)

## StaticEvaluation.tex
56. static_eval // completed (January 29)

## ASLFormal.tex
57. te_check // completed

## CatchingExceptions.tex
58. annotate_catcher // completed

## GlobalPragmas.tex
59. check_global_pragma // completed

## GlobalStorageDeclarations.tex
60. add_global_storage // completed
61. annotate_ty_opt_initial_value // completed
62. declare_global_storage // completed
63. update_global_storage // completed

## LocalStorageDeclarations.tex
64. annotate_local_decl_item // completed
65. check_is_not_collection // completed

## PatternMatching.tex
66. annotate_pattern // completed

## PrimitiveOperations.tex
67. binary_to_unsigned // completed

## Types.tex
68. is_unconstrained_integer // completed (January 28)
69. is_parameterized_integer // completed (January 28)
70. is_well_constrained_integer // completed (January 28)
71. annotate_constraint // completed (January 28)
72. annotate_symbolically_evaluable_expr // completed (January 29)
73. annotate_type // completed (January 29)
    BUGFIX: ty' in premise should be tys'
74. check_underlying_integer // completed (January 29)
75. get_variable_enum // completed (January 29)

## SymbolicEquivalenceTesting.tex
76. sym_mul_expr // completed Janury 28

## RelationsOnTypes.tex
77. annotate_constraint_binop // completed
78. apply_binop_types // completed
79. apply_unop_type // completed
80. binop_is_exploding // completed
81. bitfields_included // completed
82. check_bits_equal_width // completed
83. check_structure_label // completed
84. check_type_satisfies // completed
85. explode_constraint // completed
86. explode_intervals // completed
87. field_type // completed
88. filter_reduce_constraint_div // completed
89. get_bitvector_const_width // completed
90. get_bitvector_width // completed
91. get_literal_div_opt // completed
92. get_well_constrained_structure // completed
93. interval_too_large // completed
94. is_subtype // completed
95. mem_bfs // completed
96. negate_constraint // completed
97. subtype_satisfies // completed
98. supers // completed
99. to_well_constrained // completed
100. type_satisfies // completed (January 28)
101. precision_join // completed (January 28)
102. reduce_to_z_opt // completed (January 28)
103. lowest_common_ancestor // completed (January 28)
104. named_lowest_common_ancestor // completed (January 28)
    BUGFIX: in FOUND case the result should wrapped in a `Some`

## SideEffects.tex
105. check_symbolically_evaluable // completed (January 28)
106. is_symbolically_evaluable // completed (January 28)
107. ses_for_subprogram // completed (January 28)
108. ses_gdk // completed (January 28)
109. ses_is_pure // completed (January 28)
110. ses_is_readonly // completed (January 28)
111. ses_ldk // completed (January 28)
112. side_effect_is_pure // completed (January 28)
113. side_effect_is_readonly // completed (January 28)
114. side_effect_is_symbolically_evaluable // completed (January 28)
115. remove_local_effects // new relation, missing translation

## RelationsOnTypes.tex
116. refine_constraint_by_sign // pending translation
117. refine_constraint_for_div // *** DO NOT TRANSLATE ***
118. refine_constraints // *** DO NOT TRANSLATE ***
119. binop_filter_rhs // *** DO NOT TRANSLATE ***

## Specifications.tex
120. add_subprogram_decls // completed (January 30)
121. annotate_decl_comps // completed (January 30)
122. build_dependencies // completed (January 30)
    BUGFIX: `def_enum_labels` returns a set, not a list.
123. check_implementations_unique // completed (January 30)
124. decl_dependencies // completed (January 30)
    BUGFIX: the output is a list, not a set.
125. declare_subprograms // completed (January 30)
    BUGIX: signature output was missing side effects.
    BUGFIX: `declare_one_func` invoked without side-effects components.
126. def_decl // completed (January 30)
127. def_enum_labels // completed (January 31)
128. override_subprograms // completed (January 31)
129. override_decls_sort // completed (January 31)
130. process_overrides // completed (February 5)
131. rename_subprograms // completed (January 31)
132. signatures_match // completed (January 31)
133. type_check_ast // translation success: pending rendering
    requires updating the AST to support general field expressions.
134. type_check_mutually_rec // translation success: pending rendering
    requires updating the AST to support general field expressions (to replace `local_env_of`).
135. typecheck_decl // completed (January 31)
136. use_bitfield // completed (January 31)
137. use_catcher // completed (January 31)
138. use_constraint // completed (January 31)
139. use_decl // completed (January 31)
    BUGFIX: typed_identifier/parameter pairs are tuples; use list_combine to access types.
140. use_expr // completed (January 31)
141. use_ldi // completed (January 31)
142. use_lexpr // completed (January 31)
    BUGFIX: `LE_Var` returns an element, not a set.
143. use_pattern // completed (January 31)
    TODO: improve when aslspec supports matching multiple variants
144. use_slice // completed (January 31)
    TODO: improve when aslspec supports matching multiple variants
145. use_stmt // completed (January 31)
    TODO: improve when aslspec supports matching multiple variants
    BUGFIX: `index_name` instead of `Other(index_name)`
146. use_subtypes // completed (January 31)
147. use_ty // completed (January 31)

## StaticEvaluation.tex
148. static_env_to_env // translation success: pending rendering
    TODO: currently type subsumption for functions is too weak to support the translation.
    BUGFIX: the rule hasn't updated pending_calls and did not store the storage in the `storage` field
    (the G component of dynamic global environments was updated and this function went out of sync).

## SubprogramCalls.tex
149. annotate_call // completed (January 30)
150. annotate_call_actuals_typed // completed (January 30)
151. annotate_exprs // completed (January 29)
152. annotate_ret_ty // completed (January 29)
153. call_type_matches // completed (January 29)
154. can_omit_stdlib_param // requires re-translation when aslspec supports redefinition of variables
155. check_args_typesat // completed (January 29)
156. check_params_typesat // completed (January 29)
157. filter_call_candidates // completed (January 29)
    BUGFIX: the function doesn't need a short-circuit expression as it never errors.
    BUGFIX: `candidates` is a set, not a list.
158. has_arg_clash // completed (January 29)
159. insert_stdlib_param // compelted (February 4)
    BUGFIX: missing `tenv` argument
    aslspec bug: `List.for_all2`
160. rename_ty_eqs // completed (January 29)
161. subprogram_for_signature // completed (February 4)
    BUGFIX: G.overloaded_subprograms does returns names but no side-effects.
    BUGFIX: need to correctly retrieve side effects for the found candidate.
163. subst_constraint // completed (January 29)
164. subst_expr // completed (January 29)
    BUGFIX: `E_Call` does not have three components but rather one (`call`).
165. subst_expr_normalize // completed (January 29)
166. type_clashes // completed (January 29)

## SubprogramDeclarations.tex
167. add_new_func // completed (January 29)
168. allowed_abs_configs // completed (January 29)
169. annotate_and_declare_func // completed (January 29)
170. annotate_args // completed (January 29)
    BUGFIX: wrong grouping of expressions for `annotate_args` premise
171. annotate_func_sig // completed (January 29)
    BUGFIX: signature was missing side-effects in the output type.
    BUGFIX: include recursion-limit side effects in ses_with_params.
172. annotate_one_arg // completed (January 29)
    BUGFIX: `annotate_type` missing first argument.
173. annotate_one_param // completed (January 29)
    BUGFIX: signature misses side-effects in output
174. annotate_params // completed (January 29)
    BUGFIX: signature misses side-effects in output
175. annotate_return_type // completed (January 29)
    BUGFIX: return type argument is not a type but rather an optional ty.
    BUGFIX: `annotate_type` requires an extra first Boolean argument.
176. annotate_subprogram // completed (January 29)
177. approx_stmt // completed (January 29)
    BUGXI: in `S_Cond` case attempting to union abstract configurations and statements.
178. check_control_flow // completed (January 29)
179. check_param_decls // completed (January 29)
    BUGFIX: attempt to return undefined variable `b`
180. check_subprogram_purity // completed (January 30)
181. declare_one_func // completed (January 30)
    BUGFIX: LaTeX orders add_new_func args as (tenv, qualifier, name, args, type); use the spec signature.
182. extract_parameters // completed (January 30)
183. func_sig_types // completed (January 30)
184. params_of_constraint // completed (January 30)
185. params_of_expr // completed (January 30)
186. paramsofty // completed (January 30)
    BUGFIX: `WellConstrained` doesn't have a Boolean component.
187. subprogram_clash // completed (January 30)
    BUGFIX: `subprogram_clash` needs to be an optional.
188. subprogram_types_clash // completed (January 30)

## SymbolicEquivalenceTesting.tex
189. add_polynomials // completed (January 31)
190. array_length_equal // completed (January 31)
    BUFIX: signature missed `tenv`
191. bitfield_equal // completed (January 31)
192. bitfields_equal // completed (January 31)
193. bitwidth_equal // completed (January 31)
194. compare_monomial_bindings // completed (February 1)
    BUGFIX: inputs are not monomials but rather unitary monomials.
195. constraint_equal // completed (February 1)
196. constraints_equal // completed (February 1)
197. expr_equal // completed (February 1)
198. expr_equal_case // completed (February 1)
    BUGFIX: `E_Call` was missing comparison of parameters.
555. pattern_equal // completed (February 5)
199. expr_equal_norm // completed (February 1)
200. monomial_to_expr // completed (February 1)
201. monomials_to_expr // completed (February 1)
202. mul_monomials // completed (February 1)
203. mul_polynomials // completed (January 30)
204. normalize // completed (February 1)
205. normalize_opt // completed (February 1)
206. polynomial_divide_by_term // completed (February 1)
207. polynomial_to_expr // completed (February 1)
208. reduce_constraint // completed (February 1)
    BUGFIX: missing short-circuits for type errors
209. reduce_constraints // completed (February 1)
210. slice_equal // completed (February 1)
    Consider optimizing using "multi-match"
211. slices_equal // completed (February 1)
212. sym_add_expr // completed (February 1)
213. to_ir // completed (February 1)
214. type_equal // completed (February 1)
    BUGFIX: in `TArray` case we need to apply `array_length_equal` to the array indices, not `expr_equal`
215. type_of // completed (February 1)
    BUGFIX: `loca_storage_types` returns a pair, not just a type.
216. unitary_monomials_to_expr // completed (February 1)
    BUGFIX: In `exp_gt_two`, we need `> 2`, not `>= 2`, to avoid overlap with previous cases.

## SymbolicSubsumptionTesting.tex
217. apply_binop_extremities // translation success: pending rendering
        Requires enhancing quantifying operators to allow `list_map` to decompose its bound element.
218. approx_bottom_top // completed (February 1)
219. approx_constraint // completed (February 1)
220. approx_constraint_binop // completed (February 1)
221. approx_constraints // completed (February 1)
222. approx_expr // completed (February 2)
    BUGFIX: `BINOP_PRECISE` case was missing `intset_to_constraints`.
223. approx_expr_max // completed (February 2)
224. approx_expr_min // completed (February 2)
225. approx_type // completed (February 2)
226. constraint_binop // completed (February 2)
    TODO: improve cases involving `list_cross` once quantifying operators like `list_map`
    can be applied to bound expressions, not just bound variables.
227. constraint_mod // completed (February 2)
228. constraint_pow // completed (February 2)
229. intset_to_constraints // completed (February 2)
230. make_interval // completed (February 2)
231. possible_extremities_left // completed (February 2)
232. possible_extremities_right // completed (February 2)
233. symdom_eval // completed (February 2)
234. symdom_normalize // completed (February 2)
235. symdom_of_constraint // completed (February 2)
236. symdom_of_type // completed (February 2)
237. symdom_of_width_expr // completed (February 2)
238. symdom_subset // completed (February 2)
239. symdom_subset_unions // completed (February 2)

## TypeDeclarations.tex
240. annotate_expr_opt // completed (February 2 - removed)
241. annotate_extra_fields // completed (January 30)
    BUGFIX: third output component had wrong type.
    BUGFIX: wrongly sharing variable names.
242. annotate_type_opt // completed (February 2 - removed)
243. declare_const // completed (January 30)
244. declare_enum_labels // completed (January 30)
245. declare_type // completed (January 30)
    BUGFIX: `declare_enum_labels` returns a type environment, not a global static environment.
246. declared_type // completed (January 30)

## TypeSystemUtilities.tex
247. add_global_constant // completed (February 4)
248. add_global_immutable_expr // completed (February 4)
249. add_immutable_expression // completed (February 4)
250. add_local // completed (February 4)
251. add_local_immutable_expr // completed (February 4)
252. add_subprogram // completed (February 4)
253. add_type // completed (February 4)
254. check_no_duplicates // completed (February 4)
255. check_var_not_in_env // completed (February 4)
256. check_var_not_in_genv // completed (February 4)
257. find_bitfield_opt // completed (February 4)
258. is_global_undefined // completed (February 4)
259. is_local_undefined // completed (February 4)
260. is_undefined // completed (February 4)
261. lookup_immutable_expr // completed (February 4)
262. should_remember_immutable_expression // completed (February 4)
263. type_of_array_length // completed (February 4)
264. with_empty_local // completed (February 4)

## Bitfields.tex
265. select_indices_by_slices // translation success: pending rendering

# Semantics Relations/Functions

## Bitfields.tex
266. eval_slice_expr // completed

## Slicing.tex
267. eval_slice // completed

## Statements.tex
268. eval_stmt // completed

## SubprogramCalls.tex
269. match_func_res // completed


## ASLFormal.tex
270. de_check // completed (February 5)

## AssignableExpressions.tex
271. assign_bitvector_fields // completed (February 3)
272. check_non_overlapping_slices // completed (February 3)
273. check_two_ranges_non_overlapping // completed (February 3)
274. eval_lexpr // completed (February 3)
    BUGFIX: in `case LESetArray` "include the RHS graph g when updating the array."
    BUGFIX: in `LESetFields` the first transition is all wrong, e.g., refers to `rm_record_new`, which is undefined.
275. eval_multi_assignment // completed (February 3)

## BlockStatements.tex
276. eval_block // completed (February 3)
277. pop_local_scope // completed (February 3)

## CatchingExceptions.tex
278. eval_catchers // completed (February 4)
279. find_catcher // completed (February 4)
    BUGFIX: use of `subtypes` isn't correct - it's not a function but rather a field in the static environment.
    Should use `is_subtype` instead of `

## Expressions.tex
280. eval_expr // completed (February 3)
    BUGFIX: in `EGetTupleItem`, the order of tuple value and index for `get_index` is reversed.
281. eval_expr_list // completed (February 3)
282. eval_expr_sef // completed (February 3)
283. is_constraint_sat // completed (February 3)
284. is_val_of_type // completed (February 3)
    BUGFIX: should be applied to typed `WellConstrained` not the untyped variant.

## GlobalStorageDeclarations.tex
285. declare_global // completed (February 4)
286. eval_globals // completed (February 4)

## LocalStorageDeclarations.tex
287. declare_ldi_tuple // completed (February 4)
288. eval_local_decl // completed (February 4)

## PatternMatching.tex
289. eval_pattern // completed (February 4)
    BUGFIX: LaTeX uses evalexprsef on a pattern; should use eval_pattern.
290. mask_match // completed (February 4)

## PrimitiveOperations.tex
291. eval_binop // completed (February 2)
292. eval_unop // completed (February 2)

## SemanticsUtilities.tex
293. concat_bitvectors // completed (February 4)
294. declare_local_identifier // completed (February 4)
295. declare_local_identifier_m // completed (February 4)
296. declare_local_identifier_mm // completed (February 4)
    BUGFIX: LaTeX drops {x} in the declare_local_identifier_m premise.
297. decr_pending_calls // completed (February 4)
    BUGFIX: the dynamic environment is a record, not a pair.
298. get_field // completed (February 4)
299. get_index // completed (February 4)
    BUGFIX: `vec` is a native vector, not a list, so `|vec|` can't be applied directly.
300. get_pending_calls // completed (February 4)
301. incr_pending_calls // completed (February 4)
302. max_pos_of_slice // completed (February 4)
    BUGFIX: `s` and `l` are not integers but rather native integers, so can't directly compare to 0 and perform arithmetic.
303. read_from_bitvector // completed (February 4)
304. read_identifier // completed (February 4)
305. remove_local // completed (February 4)
306. set_field // completed (February 4)
307. set_index // completed (February 4)
308. set_pending_calls // completed (February 4)
    BUGFIX: the output type should have been `global_dynamic_envs`, not `dynamic_envs`
309. slices_to_positions // completed (February 4)
310. write_identifier // completed (February 4)
311. write_to_bitvector // completed (February 4)

## Specifications.tex
312. build_genv // completed (February 4)
313. eval_spec // completed (February 4)
    BUGFIX: signature was missing diverging configurations

## Statements.tex
314. eval_expr_list_m // completed (February 3)
315. eval_for // completed (February 3)
316. eval_for_loop // completed (February 4)
    BUGFIX: missing short-circuits for `eval_for_step`
317. eval_for_step // completed (February 4)
318. eval_limit // completed (February 3)
319. eval_loop // completed (February 4)
    BUGFIX: relation signature was missing `TDiverging`
320. lexpr_is_var // completed (February 3)
321. literal_to_string // completed (February 3)
322. output_to_console // completed (February 3)
323. tick_loop_limit // completed (February 3)
324. write_folder // completed (February 3)

## SubprogramCalls.tex
325. assign_args // completed (February 3)
326. check_recurse_limit // completed (February 3)
327. eval_call // completed (February 5)
    BUGFIX: include the subprogram execution graph in each return value's graph.
    BUGFIX: in the throwing case: keep parameter evaluation order consistent with the normal case.
328. eval_subprogram // completed (February 4)
    BUGFIX: `tenv.G.subprograms` also returns side effects.
329. read_value_from // completed (February 3)
    BUGFIX: LaTeX swaps the order of arguments to read_identifier.
