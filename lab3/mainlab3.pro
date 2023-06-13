implement main
    open core, stdio

domains
    category = сладкое; молоко; овощиифрукты; соусы; хлеб; мясоирыба; чайикофе.
    weigth = г; л; шт; кг.

class facts - storeFacts
    продукт : (integer ProductID, string ProductName, category ProductCategory, real ProductPrice, weigth ProductWeigthAmount).
    склад : (integer ProductID, string ProductName, real ProductStockPrAmount).
    клиент : (integer ClientID, string ClientName, string ClientNumber, string ClientStatus, real ClientAge).
    кассир : (integer WorkerID, string WorkerName, real WorkerAge, real WorkerWorkTimeAge, real WorkerSalary, string WorkerStatus).
    корзина : (integer BasketID, string ClientNumber, real ClientAverageBasketAmount).
    популярность : (integer ProductID, integer ProductAverageRate).
    отзывы : (integer ProductID, real ProductRewievPrAmount).
    поставщик : (integer ProviderID, integer ProductID, string ProviderName, string ProviderAdress, string ProviderNumber).
    скидки : (integer ProductID, string SaleName, real SaleAmount).
    чек : (string BoughtDay, integer ClientID, integer ProductID, real ProductAmountBuy, integer WorkerID, real BoughtTime).
    блюдо : (integer DishID, string DisnName, integer ProductDish1, integer ProductDish2).
    составблюда : (integer DishID, integer ProductID, real ProductDishAmount).
    калор : (integer ProductID, real ProductCaloriesPerKG).

class predicates  %Вспомогательные предикаты
    длина : (A*) -> integer N.
    сумма : (real* List) -> real Sum.
    среднее : (real* List) -> real Average determ.

clauses
    длина([]) = 0.
    длина([_ | T]) = длина(T) + 1.

    сумма([]) = 0.
    сумма([H | T]) = сумма(T) + H.

    среднее(L) = сумма(L) / длина(L) :-
        длина(L) > 0.

class predicates
    общий_чек : (string ClientName) -> real ClientSum nondeterm.
    опред_продукк_в_чеке_в_какой_день : (string ProductName) -> string* BoughtDay nondeterm.
    средний_возраст_всех_клиентов_работника : (string WorkerName) -> real ClientAge nondeterm.

clauses
    общий_чек(X) =
            сумма(
                [ ProductPrice * SaleAmount / 100 * ProductAmountBuy ||
                    чек(_, ClientID, ProductID, ProductAmountBuy, _, _),
                    продукт(ProductID, _, _, ProductPrice, _),
                    скидки(ProductID, _, SaleAmount)
                ]) :-
        клиент(ClientID, X, _, _, _).

    опред_продукк_в_чеке_в_какой_день(X) = CheckOprPR :-
        продукт(ProductID, X, _, _, _),
        CheckOprPR = [ BoughtDay || чек(BoughtDay, _, ProductID, _, _, _) ].

    средний_возраст_всех_клиентов_работника(X) =
            среднее(
                [ ClientAge ||
                    чек(_, ClientID, _, _, WorkerID, _),
                    клиент(ClientID, _, _, _, ClientAge)
                ]) :-
        кассир(WorkerID, X, _, _, _, _).

clauses
    run() :-
        console::init(),
        file::consult("../storefacts.txt", storeFacts),
        fail.

    run() :-
        X = 'Алексей',
        L = общий_чек(X),
        writef("Общий счёт клиента % составил %.", X, L),
        writef("\n"),
        nl,
        fail.

    run() :-
        X = 'Бананы',
        writef("Продукт : %", X),
        writef("  был приобретен в:  %", опред_продукк_в_чеке_в_какой_день(X)),
        writef("\n"),
        nl,
        fail.

    run() :-
        X = 'Екатерина',
        writef("Средний возраст всех клиентов работника : %", X),
        write("  равен :   ", средний_возраст_всех_клиентов_работника(X)),
        nl,
        fail.

    run() :-
        succeed.

end implement main

goal
    console::runUtf8(main::run).
