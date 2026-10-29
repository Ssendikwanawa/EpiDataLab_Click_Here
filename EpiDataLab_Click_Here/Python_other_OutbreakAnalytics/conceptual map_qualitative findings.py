import networkx as nx
import matplotlib.pyplot as plt

# Define themes and subthemes (edges)
edges = [
    ("Enablers", "Pathways to Leadership", "Mentorship &\nProfessional Development",
        "Partnership &\nMulti-sectoral Collaboration",
        "Progressive leadership\nroles from Jnr to Senior",
        "Guidance from Snr\ncolleague & peers",
        "Engagements with\nWHO, Red Cross, etc."),
    ("Bottlenecks", "Political &\nCultural Navigation",
        "Systemic Bottlenecks\nUndermining Leadership Impact",
        "Influence of political\n& belief systems",
        "Operational Constraints &\nresource gaps"),
    ("Adaptive Strategies", "Future-Oriented\nLeadership Strengthening",
        "Adaptive Strategies\nfor Complex Emergencies",
        "Invest in\nLeadership Devt",
        "Innovating under\npressure & uncertainty"),

]
# Create graph
G = nx.DiGraph()
G.add_edges_from(edges)
# Define node color mapping
# Define node color mapping
color_map = {}
for node in G.nodes():
    if node in [
        "Enablers", "Pathways to Leadership", "Mentorship &\nProfessional Development",
        "Partnership &\nMulti-sectoral Collaboration",
        "Progressive leadership\nroles from Jnr to Senior",
        "Guidance from Snr\ncolleague & peers",
        "Engagements with\nWHO, Red Cross, etc."
    ]:
        color_map[node] = "cadetblue"
    elif node in [
        "Bottlenecks", "Political &\nCultural Navigation",
        "Systemic Bottlenecks\nUndermining Leadership Impact",
        "Influence of political\n& belief systems",
        "Operational Constraints &\nresource gaps"
    ]:
        color_map[node] = "red"
    elif node in [
        "Adaptive Strategies", "Future-Oriented\nLeadership Strengthening",
        "Adaptive Strategies\nfor Complex Emergencies",
        "Invest in\nLeadership Devt",
        "Innovating under\npressure & uncertainty"
    ]:
        color_map[node] = "blue"
    else:
        color_map[node] = "lightgray"

# Extract node colors in order
node_colors = [color_map[node] for node in G.nodes()]

# Draw graph
plt.figure(figsize=(12, 8))
pos = nx.spring_layout(G, k=0.5, seed=42)
nx.draw(
    G, pos,
    with_labels=True,
    node_color=node_colors,
    node_size=3000,
    font_size=9,
    font_weight='bold',
    edge_color='gray'
)
plt.title("", fontsize=14, weight='bold')
plt.show()
