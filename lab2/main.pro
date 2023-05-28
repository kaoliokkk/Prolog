% Copyright

implement main
    open core, stdio

domains
    category = сладкое; молоко; овощиифрукты; соусы; хлеб; мясоирыба; чайикофе.
    weights = шт; л; кг; г.

class facts - factsss
    продукт : (integer Id, string Название, category Катерогия, real Цена, weights Weight).
    dish : (integer IdБлюда, string Название, integer P1, integer P2).
    dishcomp : (integer IdБлюда, integer IdПродукта, real K1).
    client : (integer IDКлиента, string Имя, string Номер, string Статус).
    check : (string День, integer IDКлиента, integer ID, integer IDCol).
    calor : (integer IDP, integer Calor).

class facts
    s : (real Sum) single.

clauses
    s(0).

class predicates
    состав : (string Название_блюда) nondeterm.
    покупки_в_опреддень : (string День) nondeterm.
    сумма_за_опреддень : (string День) nondeterm.
    кто_и_что_купил_в_опред_день : (string День) nondeterm.
    сколько_потратил_клиент_за_неделю : (string Имя) nondeterm.

clauses
    состав(X) :-
        dish(IdБлюда, X, _, _),
        writef("Состав %:\n", X),
        dishcomp(IdБлюда, IdПр, K1),
        продукт(IdПр, НазваниеПродукта, _, _, Weight),
        writef("\t%\n", НазваниеПродукта),
        writef("\t%\n", K1),
        writef("\t%\n", Weight),
        calor(IdПР, Calor),
        assert(s(0)),
        s(Sum),
        assert(s(Sum + K1 / 1000 * Calor)),
        fail.
    состав(X) :-
        dish(_, X, _, _),
        s(Sum),
        writef("\tКалорийность этих продуктов: %\n", Sum).

    покупки_в_опреддень(X) :-
        writef("Покупки в %:\n", X),
        check(X, _, ID, IDCol),
        продукт(ID, НазваниеПродукта, _, _, Weight),
        writef("\t%\n", НазваниеПродукта),
        writef("\t%\n", IDCol),
        writef("\t%\n", Weight),
        fail.
    покупки_в_опреддень(X) :-
        check(X, _, _, _).

    сумма_за_опреддень(X) :-
        write("Сумма чеков за %:\n", X),
        assert(s(0)),
        check(X, _, ID, IDCol),
        продукт(ID, _, _, Цена, _),
        s(Sum),
        assert(s(Sum + Цена * IDCol)),
        fail.
    сумма_за_опреддень(X) :-
        check(X, _, _, _),
        s(Sum),
        writef("\t% cумма %:\n", X, Sum),
        nl.

    кто_и_что_купил_в_опред_день(X) :-
        writef("Покупки в %:\n", X),
        check(X, IDКлиента, ID, IDCol),
        client(IDКлиента, Имя, _, _),
        продукт(ID, НазваниеПродукта, _, _, Weight),
        writef("\t%\n", Имя),
        writef("\t%\n", НазваниеПродукта),
        writef("\t%\n", IDCol),
        writef("\t%\n", Weight),
        fail.
    кто_и_что_купил_в_опред_день(X) :-
        check(X, _, _, _).

    сколько_потратил_клиент_за_неделю(X) :-
        writef("Сколько потратил клиент %:\n", X),
        client(IDКлиента, X, _, _),
        check(_, IDКлиента, ID, IDCol),
        продукт(ID, _, _, Цена, _),
        assert(s(0)),
        s(Sum),
        assert(s(Sum + Цена * IDCol)),
        fail.
    сколько_потратил_клиент_за_неделю(X) :-
        client(IDКлиента, X, _, _),
        s(Sum),
        writef("\t% потратил в нашем машагазине %\n", X, Sum),
        nl.

clauses
    run() :-
        file::consult("../factstext.txt", factsss),
        fail.

    run() :-
        состав("Шоколадный бисквит с кремом из конфеты Аленка, украшенный мандаринами."),
        fail.

    run() :-
        покупки_в_опреддень("Mon"),
        fail.

    run() :-
        сумма_за_опреддень("Tue"),
        fail.

    run() :-
        кто_и_что_купил_в_опред_день("Tue"),
        fail.

    run() :-
        сколько_потратил_клиент_за_неделю("Екатерина"),
        fail.

    run().

end implement main

goal
    console::runUtf8(main::run).
