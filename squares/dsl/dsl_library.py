from squares.dsl.dsl_builder import *

table_value = DSLValue('Table',
                       [
                           ('col', 'int'),
                           ('row', 'int'),
                           # ('columns', 'bv')
                       ])

table_select_value = DSLValue('TableSelect',
                              [
                                  ('col', 'int'),
                                  ('row', 'int'),
                                  # ('columns', 'bv')
                              ])

natural_join_function = DSLFunction('natural_join',
                                    'Table r',
                                    ['Table a', 'Table b'],
                                    [
                                        "col(r) <= col(a) + col(b)",
                                        # "columns(r) == columns(a) | columns(b)",
                                        # f"columns(a) & columns(b) != *0"
                                    ])

natural_join3_function = DSLFunction('natural_join3',
                                     'Table r',
                                     ['Table a', 'Table b', 'Table c'],
                                     [
                                         "col(r) < col(a) + col(b) + col(c)",
                                         # "columns(r) == columns(a) | columns(b) | columns(c)",
                                         # f"columns(a) & columns(b) != *0",
                                         # f"(columns(a) | columns(b)) & columns(c) != *0"
                                     ])

natural_join4_function = DSLFunction('natural_join4',
                                     'Table r',
                                     ['Table a', 'Table b', 'Table c', 'Table d'],
                                     [
                                         "col(r) < col(a) + col(b) + col(c) + col(d)",
                                         # "columns(r) == columns(a) | columns(b) | columns(c) | columns(d)",
                                         # f"columns(a) & columns(b) != *0",
                                         # f"(columns(a) | columns(b)) & columns(c) != *0",
                                         # f"(columns(a) | columns(b) | columns(c)) & columns(d) != *0"
                                     ])

inner_join_function = DSLFunction('inner_join',
                                  'Table r',
                                  ['Table a', 'Table b', 'JoinCondition c'],
                                  [
                                      "col(r) <= col(a) + col(b)"
                                  ])

anti_join_function = DSLFunction('anti_join',
                                 'Table r',
                                 ['Table a', 'Table b', 'JoinCols c'],
                                 [
                                     "col(r) == 1",
                                     'row(r) <= row(a)'
                                 ])

left_join_function = DSLFunction('left_join',
                                 'Table r',
                                 ['Table a', 'Table b'],
                                 [
                                     'col(r) <= col(a) + col(b)',
                                     'row(r) == row(a)',
                                     # "columns(r) == columns(a) | columns(b)"
                                 ])

union_function = DSLFunction('union',
                             'Table r',
                             ['Table a', 'Table b'],
                             [
                                 'col(r) <= col(a) + col(b)',
                                 'row(r) == row(a) + row(b)',
                                 # "columns(r) == columns(a) | columns(b)"
                             ])

intersect_function = DSLFunction('intersect',
                                 'Table r',
                                 ['Table a', 'Table b', 'Col c'],
                                 [
                                     'col(r) == 1',
                                     'row(r) <= row(a)'
                                 ])

semi_join_function = DSLFunction('semi_join',
                                 'Table r',
                                 ['Table a', 'Table b'],
                                 [
                                     'col(r) == col(a)',
                                     'row(r) <= row(a)'
                                 ])

select_function = DSLFunction('select',
                              'TableSelect r',
                              ['Table a', 'SelectCols c', 'Distinct d'],
                              [
                                  'row(r) <= row(a)',
                                  'col(r) <= col(a)',
                                  # f"columns(r) & columns(a) != *0",
                                  # f"columns(r) & ~columns(a) == *0",
                              ])

unite_function = DSLFunction('unite',
                             'Table r',
                             ['Table a', 'Col c', 'Col d'],
                             [
                                 'row(r) <= row(a)',
                                 'col(r) <= col(a)'
                             ])

filter_function = DSLFunction('filter',
                              'Table r',
                              ['Table a', 'FilterCondition f'],
                              [
                                  'row(r) <= row(a)',
                                  'col(r) == col(a)',
                                  # 'columns(r) == columns(a)'
                              ])

filters_function = DSLFunction('filters',
                               'Table r',
                               ['Table a', 'FilterCondition f', 'FilterCondition g', 'Op o'],
                               [
                                   'row(r) <= row(a)',
                                   'col(r) == col(a)'
                                   # 'columns(r) == columns(a)'
                               ])

summarise_grouped_function = DSLFunction('summarise',
                                         'Table r',
                                         ['Table a', 'SummariseCondition s', 'Cols b'],
                                         [
                                             'row(r) <= row(a)',
                                             'col(r) <= 3'
                                         ])
