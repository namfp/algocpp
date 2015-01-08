__author__ = 'hnng'



def compute(total, oods):
    if sum([ood['budget'] for ood in oods]) < total:
        return
    n = len(budgets)
    average = total / n
    for ood in oods:
        if ood['budget'] < average:
            ood['pay'] = ood['budget']
            return
    pass


    # for k, budget in enumerate(budgets):
    #     dict_budgets[k] = {
    #         'budget': budget,
    #         'pay': 0
    #     }