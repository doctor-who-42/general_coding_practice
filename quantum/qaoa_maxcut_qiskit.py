'''
    File name: qaoa_maxcut_qiskit.py
    Author: Richard A. Wolf
    Date created: 19/07/2022
    Date last modified: 19/07/2022
    Python Version: 3.10.5
    Notes : based on https://arxiv.org/abs/1411.4028 and
     https://qiskit.org/textbook/ch-applications/qaoa.html#Combinatorial-Optimization-Problem
'''

from copy import deepcopy
from networkx import Graph
from numpy import repeat
from qiskit import QuantumCircuit
from qiskit import Aer, execute
from qiskit.circuit import Parameter
from scipy.optimize import minimize
from typing import List

###########################################################
################# INITIAL DEFINITIONS #####################
###########################################################
NB_QBS = 4
P = 1 # depth of the QAOA circuit, i.e. number of problem+mixing layers repetitions
NB_PARAMS = P*2 # number of gammas and betas for the whole circuit

NB_SHOTS = 1024
NB_OPTI_STEPS = 100

BACKEND = Aer.get_backend('aer_simulator')
BACKEND.shots = NB_SHOTS

# The graph problem
G = Graph()
G.add_nodes_from([0, 1, 2, 3])
G.add_edges_from([(0, 1), (1, 2), (2, 3), (3, 0)])


def maxcut_obj(bitstring:str, G:Graph) -> int:
    """Returns the number of edges shared between the 2 partitions """
    obj = 0
    for i, j in G.edges():
        if bitstring[i] != bitstring[j]:
            obj -= 1
    return obj


def compute_expectation(counts, G):
    """Computes expectation value based on measurement results"""
    avg = 0
    sum_count = 0
    for bitstring, count in counts.items():
        obj = maxcut_obj(bitstring, G)
        avg += obj * count
        sum_count += count
    return avg/sum_count


def create_qaoa_circ(G:Graph, theta:List, p=P, nb_qbs=NB_QBS) -> QuantumCircuit:
    """ Creates a parametrized qaoa circuit. """
    qc = QuantumCircuit(nb_qbs)
    
    beta = theta[:p]
    gamma = theta[p:]
    
    for i in range(0, nb_qbs): # Hadamard init
        qc.h(i)
    
    for irep in range(0, p):

        for pair in list(G.edges()): # problem unitary
            qc.rzz(2 * gamma[irep], pair[0], pair[1])

        for i in range(0, nb_qbs): # mixer unitary
            qc.rx(2 * beta[irep], i)
            
    qc.measure_all()
    return qc


def get_expectation(G:Graph, p:int, shots=NB_SHOTS, 
                    backend=BACKEND) -> QuantumCircuit:
    """ Runs PQC on backend. """
    
    def execute_circ(theta):
        qc = create_qaoa_circ(G, theta)
        counts = backend.run(qc, seed_simulator=10, 
        nshots=shots).result().get_counts()
        return compute_expectation(counts, G)
    
    return execute_circ



###########################################################
######################### MAIN ############################
###########################################################

expectation = get_expectation(G, p=P)

res = minimize(expectation, repeat(a=1.0, repeats=NB_PARAMS), method='COBYLA')
theta = res.x
beta = theta[:P]
gamma = theta[P:]
print("Parameters optimisation result : gammas = {}  //  betas = {}".format(gamma, beta))

qaoa = create_qaoa_circ(G, res.x)

counts = BACKEND.run(qaoa, seed_simulator=10).result().get_counts()
cpts = deepcopy(counts)

max_val = max(counts.values())
max_key = max(counts, key=counts.get)
print("The most obtained string was {} with {} occurences.".format(max_key, max_val))

del counts[max_key]
max_val2 = max(counts.values())
max_key2 = max(counts, key=counts.get)
print("The second most obtained string was {} with {} occurences ({} less than the most frequent one).".format(max_key2, max_val2, max_val-max_val2))

del counts[max_key2]
max_val3 = max(counts.values())
max_key3 = max(counts, key=counts.get)
print("The third most obtained string was {} with {} occurences ({} less than the most frequent one and {} less than the second most frequent one).".format(max_key3, max_val3, max_val-max_val3, max_val2-max_val3 ))

for c in cpts:
    print("String {} was obtained {} times".format(c, cpts[c]))