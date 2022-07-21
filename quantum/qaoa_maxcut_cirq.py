'''
    File name: qaoa_maxcut_cirq.py
    Author: Richard A. Wolf
    Date created: 14/07/2022
    Date last modified: 19/07/2022
    Python Version: 3.10.5
    Notes : based on https://arxiv.org/abs/1411.4028 and
     https://quantumai.google/cirq/experiments/qaoa/qaoa_maxcut
'''


from cirq import Circuit, H, Linspace, Moment, measure, Qid, Simulator, X, ZZ
from cirq_google import Sycamore
from cirq.contrib.svg import SVGCircuit
from matplotlib.pyplot import imshow, show, title, xlabel, ylabel
from networkx import cut_size, draw, Graph, set_edge_attributes
from numpy import argmax, array, inf, mean, pi, reshape, unravel_index, vectorize
from numpy.random import rand
from pandas import DataFrame
from sympy import Symbol
from typing import List, Tuple



###########################################################
################# INITIAL DEFINITIONS #####################
###########################################################
DEVICE = Sycamore
SIMU = Simulator()
GRID_SIZE = 3
NB_CUTS = 10
GRAPH_SIZE = 8
NB_REPS_SAMP = 10000

def compute_best_params(gamma, beta, qaoa, gs:int=GRID_SIZE):
    gamma_sweep = Linspace(gamma, 0, 2*pi, GRID_SIZE)
    beta_sweep = Linspace(beta, 0, 2*pi, GRID_SIZE)
    samples = SIMU.run_sweep(qaoa, params=gamma_sweep * beta_sweep, 
                            repetitions=NB_REPS_SAMP)

    best_idx = get_best_exp_index(samples, graph_circuit, GRID_SIZE)
    par_vals = get_parameter_vals(gamma_sweep, beta_sweep, GRID_SIZE)
    best_parameters = par_vals[best_idx]

    print("Best control parameters: {}".format(best_parameters))
    return best_parameters


def get_parameter_vals(gammas:dict, betas:dict, gs:int=GRID_SIZE) -> array:
    tups =  [tuple(y[1] for y in x) for x in (gammas * betas).param_tuples()]
    r = reshape(tups, (-1, gs, 2))
    return r


def get_best_exp_index(s:array, c:Graph,  gs:int=GRID_SIZE) -> tuple[int,int]:
    sv = reshape(s, (-1, gs)).tolist()
    estimate = vectorize(lambda s: estimate_cost(graph_circuit, s.data))
    samp_vals =  estimate(sv)
    best = unravel_index(argmax(samp_vals), samp_vals.shape)
    return best


def estimate_cost(graph: Graph, samples: DataFrame) -> float:
    """Uses the bitstring obtained from measurement to estimate 
    the cost function of QAOA """
    cost_value = 0.0

    # loop over edge pairs
    for q0, q1, d in graph.edges(data=True):
        q0_samples = samples[str(q0)]
        q1_samples = samples[str(q1)]

        # check eigenvalue sign
        q0_signs = (-1) ** q0_samples
        q1_signs = (-1) ** q1_samples
        term_signs = q0_signs * q1_signs

        term_val = mean(term_signs) * d["weight"] #add the weighted value
        cost_value += term_val

    return -cost_value
  


###########################################################
################# Max cut problem #########################
###########################################################
# select qbits on Sycamore
device_qubits = DEVICE.metadata.qubit_set
working_qubits = sorted(device_qubits)[:GRAPH_SIZE]

# 1 qubit = 1 node in the graph
graph_circuit = DEVICE.metadata.nx_graph.subgraph(working_qubits)

# initialise random weights for all edges
set_edge_attributes( graph_circuit, {e: {"weight": rand()} for e in graph_circuit.edges})



###########################################################
####################### QAOA ##############################
###########################################################
# Symbols for the rotation angles in the QAOA circuit.
gamma = Symbol("angle0")
beta = Symbol("angle1")

qaoa = Circuit(
    # uniform superposition of all nodes' states (1,0) => Hadamard
    H.on_each(graph_circuit.nodes()),
    # ZZ on each adjacent pair, changes phases (diag = e^{i phi/2}, e^{-i phi/2}, e^{-i phi/2}, e^{i phi/2})
    [ ZZ(q0, q1) ** (gamma * d["weight"]) for (q0, q1, d) in graph_circuit.edges(data=True)],
    # apply X on each node
    Moment(X(qb) ** beta for qb in graph_circuit.nodes()),
    # measure in computational basis
    (measure(qb) for qb in graph_circuit.nodes()))



###########################################################
################ Classical optimisation ###################
###########################################################

################ Single trial run #######################
gamma_value = pi / 4
beta_value = pi / 2

sample_results = SIMU.sample(qaoa, params={gamma: gamma_value, beta: beta_value},
                             repetitions=NB_REPS_SAMP)
print("Angle_0 = {} | Angle_1 = {}".format(round(gamma_value, 3), round(beta_value, 3)))
print("Estimated cost: {}".format(estimate_cost(graph_circuit, sample_results)))




###########################################################
######################### Task ############################
###########################################################
gamma_best, beta_best = compute_best_params(gamma=gamma, beta=beta, qaoa=qaoa, gs=GRID_SIZE)

best_size = -inf
best_S = set()
best_T = set()

candidate_cuts = SIMU.sample(qaoa,
                            params={gamma: gamma_best, 
                                    beta: beta_best},
                            repetitions=NB_CUTS)

for i in range(NB_CUTS):
    curr_c = candidate_cuts.iloc[i]
    qb1 = set(curr_c[curr_c == 1].index)
    S_partition = set()
    T_partition = set()
    # separate nodes according to whether 1 or 0 was measured
    for node in graph_circuit:
        if str(node) in qb1:
            S_partition.add(node) #measurement=1 => S
        else:
            T_partition.add(node) #mesurement=0 => T
    c_size = cut_size(graph_circuit, S_partition, T_partition, weight="weight")
    if c_size > best_size: #update if better is found
        best_size = c_size
        best_S = S_partition
        best_T = T_partition
    
print("Best QAOA cut size is {}".format(c_size))