# convert 5-year probability 
# to 1-year probability
prob_to_prob(p = .65, from = 5)

# convert 1-year probability 
# to 1-month probability
prob_to_prob(p = .5, to = 1/12)

# convert rate per 1000 PY
# to 5-year probability
rate_to_prob(r = 162, per = 1000, to = 5)

# convert OR to probability
or_to_prob(or = 1.9, p = .51)

# convert RR to probability
rr_to_prob(rr = 1.9, p = .51)
