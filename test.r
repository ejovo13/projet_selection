# lib.loc = "//home/ejovo/R/x86_64-pc-linux-gnu-library/4.1"
lib.loc = "/home/ejovo/.r"
library(dplyr, lib.loc = lib.loc)
library(ggplot2, lib.loc = lib.loc)
library(tidyr, lib.loc = lib.loc)
library(purrr, lib.loc = lib.loc)
# library(tidyverse, lib.loc = lib.loc)
library(tibble)
# library(magrittr)

MAX_PER_GROUP <- 4

data <- as_tibble(read.csv("final.csv"))






# Filter each column based on a rank
# Seed the generator to make the results reproducible
set.seed(1234)

pivot <- function(df) {
    df |> pivot_longer(c(Homer.vs.Donuts, Je.de.Vie, PageRank, N.Corps, Blockchain, Cables.sous.marin, Liquides.Ferro, Imprimerie.3d),
                                values_to = "rank", names_to = "sujet")
}

# I want a function to extract the nth choice for each person
data_long <- data |> pivot_longer(c(Homer.vs.Donuts, Je.de.Vie, PageRank, N.Corps, Blockchain, Cables.sous.marin, Liquides.Ferro, Imprimerie.3d),
                                values_to = "rank", names_to = "sujet")

data_long

nom_sujets <- unique(data_long$sujet)

place_left <- function(group_members) {
    l <- length(group_members)
    return(max(MAX_PER_GROUP - l, 0))
}

filter_rank <- function(df, n) {
    df |> filter(rank == n)
}

count_rank <- function(df, n) {
    sum(with(df, rank == n))
}

get_subject <- function(df, sub) {
    df |> filter(df[2] == nom_sujets[sub])
}

count_subjects_rank <- function(df, sub, n) {
    count_rank(get_subject(df, sub), n)
}

# Have a function count the number for each rank and store that in a vector
count_ranks <- function(df, sub) {

    ranks_count <- c()

    for (i in 1:length(nom_sujets)) {

        ranks_count <- c(ranks_count, count_subjects_rank(df, sub, i))

    }
    ranks_count
}

summarise_rank_counts <- function(data_long) {

    df <- tibble(rank = 1:8)

    for (i in 1:8) {

        df <- df |> add_column("s" = count_ranks(data_long, i), .name_repair="unique")

    }

    for (i in 2:9) {
        # df <- df |> mutate(as.character(i) = df[i])
        # df <- rename(df, "s2" = `s...2`)
    }

    names(df) <- c("rank", nom_sujets)
    df
}

get_subj_names <- function(data) {
    names(select(data, Homer.vs.Donuts:Imprimerie.3d))
}

subj_table <- function(data) {
    data |> select(Homer.vs.Donuts:Imprimerie.3d)
}

clean_table <- function(u) {
    u.long <- pivot(u)
    u.counts <- summarise_rank_counts(u.long)
    u.counts.long <- u.counts |> pivot_longer(c(nom_sujets), names_to = "sujet", values_to ="counts")
    u.counts.long <- u.counts.long |>
        mutate(rank = factor(rank)) |>
        mutate(sujet = factor(sujet))

    u.counts.long
    # u.counts
}

df.counts <- summarise_rank_counts(data_long)
df.counts.long <- df.counts |> pivot_longer(c(nom_sujets), names_to = "sujet", values_to = "counts")


# I need a function that will count the number of rank per subject
df.counts.long <- df.counts.long |>
    mutate(rank = factor(rank)) |>
    mutate(sujet = factor(sujet))
     #|>
    # mutate(counts = factor(counts))

# df.counts.long[df.counts.long$counts == 0] <- NA


# I should arrange the factors according to their average score

p <- df.counts.long |>
    ggplot(aes(x = rank, y = counts, fill = rank)) +
    geom_bar(stat="identity") + facet_wrap(~sujet, scales="fixed") #+
    # scale_y_discrete(breaks = waiver(), map_chr(1:12, as.character))
# p <- df.counts.long |> ggplot(aes(x = rank, y = counts, fill = counts)) + geom_boxplot() + facet_wrap(~sujet)
p

# sum(data_long |> filter(rank == 1))

get_col <- function(df, ncol) {
    df[[names(df)[ncol]]]
}

# data |> filter(Homer.vs.Donuts == 1)
# ?with



# Ok so now I should write the actual selection algorithm.

# First thing that we want to do is for each subject, count
# how many values there are for a given rank


add_to_list <- function(list, val) {
    list[length(list) + 1] <- val
    list
}

add_to_group <- function(group, nsub, name) {
    group[[nsub]] <- add_to_list(group[[nsub]], name)
    group
}

# Return a list of names of people who placed a subject at a given rank
get_names <- function(data, nsub, rank) {
    subjects <- data |> select(Nom, nom_sujets[nsub])
    subjects <- subjects |> filter(get_col(subjects, 2) == rank)
    subjects[["Nom"]]
}

places_left_list <- function(groups, sub, cutoff) {
    max(0, cutoff - length(groups[[sub]]))
}

condition_remove <- function(all_names, names_to_remove) {

    logic = rep(TRUE, length(all_names))

    # for (n in names_to_remove) {
    #     all_names <- all_names[all_names != n]
    # }
    # all_names

    # This should actually return
    i <- 1
    for (n in all_names) {
        logic[i] <- !(n %in% names_to_remove)
        i <- i + 1
    }

    logic

}

remove_names <- function(df, names_to_remove) {
    df |> filter(condition_remove(df$Nom, names_to_remove))
}


groups <- list()

# initialize group members
for (s in 1:8) {
    groups[[nom_sujets[s]]] <- list()
    # groups[[nom_sujets[s]]] <- ""
}

unchosen <- data

# For every rank
for (r in 1:8) {

    u_c_long <- clean_table(unchosen)

    df <- u_c_long |> filter(rank == r)

    # For every subject
    for (sub in 1:8) {

        count <- get_col(df, 3)[sub] # df[3] corresponds to the counts column
        names <- get_names(unchosen, sub, r)

        # Is there enough space for all these chickens?
        if (count <= places_left_list(groups, sub, MAX_PER_GROUP) && count != 0) {

            for (n in names) {
               groups <- add_to_group(groups, sub, n)
            }

            # And then remove those added names from data?
            unchosen <- remove_names(unchosen, names)
            print("Removing names:")
            print(names)
        } else if (length(names) != 0) {
            # If there isnt enough space, randomly choose people to add to the group.
            print("Not enough space")
            while (places_left_list(groups, sub, MAX_PER_GROUP) > 0) {


                lucky_student_index <- sample(seq_len(length(names)), size = 1)
                print("Sampled value:")
                print(lucky_student_index)

                student <- names[lucky_student_index]
                # Remove name from names
                names <- names[names != student]
                print("Removing name: ")
                print(student)

                groups <- add_to_group(groups, sub, student)
                unchosen <- remove_names(unchosen, student)

            }


        }



    }
}

# Let's rearrange the order of the items so that I can display from smallest mean to the largest

data.tr <- data |> summarise_each(funs(mean))
data.tr <- unlist(data.tr[2:9])

df.counts.long <- df.counts.long |> mutate(sujet = factor(sujet, levels = nom_sujets[order(data.tr)]))
p <- df.counts.long |>
    ggplot(aes(x = rank, y = counts, fill = sujet)) +
    geom_bar(stat="identity") + facet_wrap(~sujet, scales="fixed") #+
    # scale_y_discrete(breaks = waiver(), map_chr(1:12, as.character))

# Print the groups
groups