factorial <- function(n, acc = 1) {
    if (n == 1) acc
    else factorial(n - 1, n * acc)
}

factorial_tr_manual <- function (n, acc = 1)
{
    repeat {
        if (n <= 1)
            return(acc)
        else {
            .tailr_n <- n - 1
            .tailr_acc <- acc * n
            n <- .tailr_n
            acc <- .tailr_acc
            next
        }
    }
}

factorial_tr_automatic_1 <- function(n, acc = 1) {
    .tailr_n <- n
    .tailr_acc <- acc
    callCC(function(escape) {
        repeat {
            n <- .tailr_n
            acc <- .tailr_acc
            if (n <= 1) {
                escape(acc)
            } else {
                .tailr_n <<- n - 1
                .tailr_acc <<- n * acc
            }
        }
    })
}

factorial_tr_automatic_2 <- function(n, acc = 1) {
    .tailr_env <- rlang::get_env()
    callCC(function(escape) {
        repeat {
            if (n <= 1) {
                escape(acc)
            } else {
                .tailr_env$.tailr_n <- n - 1
                .tailr_env$.tailr_acc <- n * acc
                .tailr_env$n <- .tailr_env$.tailr_n
                .tailr_env$acc <- .tailr_env$.tailr_acc
            }
        }
    })
}

actual <- loop_transform(factorial)

microbenchmark::microbenchmark(factorial(1000),
                               factorial_tr_manual(1000),
                               factorial_tr_automatic_1(1000),
                               factorial_tr_automatic_2(1000),
                               actual(1000))


library(pmatch)
llist := NIL | CONS(car, cdr : llist)

llength <- function(llist, acc = 0) {
    cases(llist,
          NIL -> acc,
          CONS(car, cdr) -> llength(cdr, acc + 1))
}

llength_tr <- loop_transform(llength)

make_llist <- function(n) {
    l <- NIL
    for (i in 1:n) {
        l <- CONS(i, l)
    }
    l
}
test_llist <- make_llist(10)
microbenchmark::microbenchmark(llength(test_llist),
                               llength_tr(test_llist))

