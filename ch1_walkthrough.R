#### Loading and examining the network ####
data("eebo_network")
library(igraph)
g = eebo_network

# Figure 1.2
subg = subgraph(g, which(degree(g) > 25))
V(subg)$name = ""
plot(simplify(subg), 
     layout = layout_with_drl(simplify(subg)),
     vertex.color = V(subg)$clust, 
     vertex.size = log(degree(subg)))


# Table 1.1
d = degree(g)
b = betweenness(g)

# Top by degree
sort(d, decreasing = T)[1:10]

# Top by betweenness / degree
hits = which(d > 10) # To remove minor figures & outliers
sort(b[hits]/d[hits], decreasing = T)[1:10]


#### Analyzing the degree distribution ####
pk = table(d)
k = as.numeric(names(pk))
pk = as.numeric(pk)

# Figure 1.4
plot(k, pk)

# Figure 1.5
plot(log(k), log(pk))
hits = which(log(k) > 1 & log(k) < 5)
abline(lm(log(pk)[hits] ~ log(k)[hits]))

# Figures 1.6 and 1.7
role_counts = table(V(g)$role)
barplot(role_counts[c("author","bookseller","printer")])


sort(d[V(g)$role == "author"], decreasing = T)[1:10]
hits = which(V(g)$role %in% c("author","bookseller","printer"))
boxplot(d[hits] ~ V(g)$role[hits], outline = FALSE)
# Again, showing outliers
boxplot(d[hits] ~ V(g)$role[hits])

#### Change over time ####
# Set empty variables
YEAR = c()
TITLES = c()
NODES = c()
EDGES = c()
CENTRALIZATION = c()

# Begin for loop
for (i in 1500:1699) {
  print(i)
  
  # Create a subgraph for each trailing 10-year window
  start = i - 9
  end = i
  hits = which(E(g)$date >= start & E(g)$date <= end)
  subg = subgraph.edges(g, hits)
  
  # Gather basic data for each year
  YEAR = c(YEAR, end)
  TITLES = c(TITLES,length(unique(E(subg)$tcp)))
  NODES = c(NODES, vcount(subg))
  EDGES = c(EDGES, ecount(subg))
  CENTRALIZATION = c(CENTRALIZATION, centr_eigen(subg)$centralization)
}
network_data = data.frame(YEAR, TITLES, NODES, EDGES, CENTRALIZATION)

# Figures 1.8, 1.9, 1.10, and 1.11
attach(network_data)
plot(YEAR, TITLES, type="l")
plot(YEAR, NODES, type="l")
plot(YEAR, EDGES, type="l")
plot(YEAR, EDGES / NODES, type = "l")
hits = which(CENTRALIZATION > .99)
abline(v = YEAR[hits])

# Figure 1.12
hits = which(V(g)$role == "printer")
subg = subgraph(g, hits)
hits = which(degree(subg) > 5)
subg = subgraph(subg, hits)
V(subg)$name = ""
V(subg)$color = V(subg)$clust
V(subg)$size = log(degree(subg)) + 2
plot(simplify(subg))

#### Historical Periodization ####
# Limit to seven largest communities
comms = 1:7

# Table 1.2
d = degree(g)
for (i in 1:length(comms)) {
  comm = comms[i]
  hits = which(V(g)$role == "author" & V(g)$clust == comm)
  authors = sort(d[hits], decreasing = T)[1:10]
  hits = which(V(g)$role != "author" & V(g)$clust == comm)
  stationers = sort(d[hits], decreasing = T)[1:10]
  if (i == 1) {
    df = data.frame(names(authors), 
                    authors, 
                    names(stationers),
                    stationers)
  } else {
    df = cbind(df, data.frame(names(authors), 
                              authors, 
                              names(stationers),
                              stationers))
  }
}
colnames(df)

View(df)

# Examples of Shakespeare & Jonson #

# Fig 1.13
subg = induced_subgraph(g, V(g)[V(g)$clust == 7])

# Begin by selecting community of Shakespeare (clust == 1)
# subg = induced_subgraph(g, V(g)[V(g)$clust == 1])

# Then, within that subgraph, perform another community detection
wt = cluster_walktrap(subg, steps = 5)

# Limit, again, to the community Shakespeare is in
comm = wt$membership[which(wt$names == "Jonson, Ben, 1573?")]
# comm = wt$membership[which(wt$names == "Shakespeare, William, 1564-1616.")]
subg = induced_subgraph(subg, V(subg)[wt$membership == comm])

# And limit to authors only
subg = induced_subgraph(subg, V(subg)[V(subg)$role == "author"])


# But we still have 280 nodes -- so repeat process
comms = clusters(subg)
comm = comms$membership["Jonson, Ben, 1573?"]
subg = induced_subgraph(subg, V(subg)[comms$membership == comm])

# Now prepare the plot itself
V(subg)$size = 2 + log(degree(subg))
pg = simplify(subg)
V(pg)$label = V(pg)$name
plot(pg)

# Fig 1.14
# Begin by selecting community of Shakespeare (clust == 1)
subg = induced_subgraph(g, V(g)[V(g)$clust == 1])

# Then, within that subgraph, perform another community detection
wt = cluster_walktrap(subg, steps = 5)

# Limit, again, to the community Shakespeare is in
comm = wt$membership[which(wt$names == "Shakespeare, William, 1564-1616.")]
subg = induced_subgraph(subg, V(subg)[wt$membership == comm])

# And limit to authors only
subg = induced_subgraph(subg, V(subg)[V(subg)$role == "author"])

# But we still have 250 nodes -- so repeat process
comms = clusters(subg)
comm = comms$membership["Shakespeare, William, 1564-1616."]
subg = induced_subgraph(subg, V(subg)[comms$membership == comm])

# But there are a lot more authors around Shakespeare in the
# Restoration, so there's one last round of filtering we
# didn't need for Jonson
d = degree(subg)
subg = induced_subgraph(subg, V(subg)[d > 9])
V(subg)$size = 2 + log(degree(subg))
pg = simplify(subg)
V(pg)$label = V(pg)$name
plot(pg)

# Figure 1.15
clusts = V(g)$clust
comms = 1:7

for (i in comms) {
  print(i)
  comm = comms[i]
  subg = induced_subgraph(g, clusts == comm)
  dates = E(subg)$date
  label = round(mean(dates), digit = 0)
  if (i == 1) {
    df = data.frame(dates, 
                    rep(label, length(dates)), 
                    rep(comm, length(dates)))
  } else {
    df = rbind(df, data.frame(dates, 
                              rep(label, length(dates)),
                              rep(comm, length(dates))))
  }
}
colnames(df) = c("YEAR","RANGE","COMM")
df$RANGE = as.factor(df$RANGE)
df$COMM = as.factor(df$COMM)
boxplot(df$YEAR ~ df$RANGE, 
        horizontal = T, 
        # width = table(df$RANGE),
        width = table(df$COMM),
        las = 1,
        outline = F)
abline(v = 1640)
abline(v = 1660)
