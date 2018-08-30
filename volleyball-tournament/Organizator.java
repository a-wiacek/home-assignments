package turniej;

import java.io.IOException;
import java.sql.*;

class Organizator {

    private boolean login(Connection db) throws IOException, SQLException {
        String readLogin, readPassword, info = null,
               SQL = "select * from organizatorzy where login = ? and " +
                     "haslo = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            System.out.println("Panel dla organizatorów");
            System.out.println("Wpisz pustą linię, aby powrócić do menu " +
                    "głównego lub podaj swój login:");
            readLogin = Common.readLine();
            if (readLogin.equals("")) {
                statement.close();
                return false;
            }
            System.out.println("Wpisz hasło:");
            readPassword = Common.readPassword();
            statement.setString(1, readLogin);
            statement.setString(2, readPassword);
            if (statement.executeQuery().isBeforeFirst()) {
                statement.close();
                return true;
            } else {
                info = "Nieprawidłowy login lub hasło " +
                       "- spróbuj ponownie";
            }
        }
    }

    private String chooseMode(Connection db) throws IOException, SQLException {
        Common.clearView();
        String mode, SQL = "select rozpocznij_nowy_turniej()";
        if (Common.tournamentHasBegun(db)) {
            while (true) {
                Common.clearView();
                System.out.println("W bazie danych znajduje się już " +
                        "rozpoczęty turniej.");
                System.out.println("Oznacza to, że edycja drużyn jest " +
                        "zablokowana.");
                System.out.println("Możesz kontynuować ten turniej lub " +
                        "rozpocząć nowy.");
                System.out.println("Wpisz \"stary\", jeśli chcesz kontynuować" +
                        " rozpoczęty turniej.");
                System.out.println("Wpisz \"nowy\", aby usunąć wszystkie " +
                        "rozegrane mecze z bazy danych");
                System.out.println("    i przejść do menedżera drużyn. " +
                        "Drużyny i zawodnicy pozostaną");
                System.out.println("    w bazie danych.");
                mode = Common.readLine();
                if (mode.equals("stary")) {
                    return "continueTournament";
                } else if (mode.equals("nowy")) {
                    CallableStatement statement = db.prepareCall(SQL);
                    statement.execute();
                    db.commit();
                    statement.close();
                    return "editTeams";
                }
            }
        } else {
            return "editTeams";
        }
    }

    private String addTeam(Connection db) throws IOException, SQLException {
        String info = null, name, returnValue,
                SQL1 = "select * from druzyny where nazwa = ?",
                SQL2 = "insert into druzyny values (?)";
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                          statement2 = db.prepareStatement(SQL2);
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            System.out.println("Nazwa może zawierać od 1 do 30 znaków.");
            System.out.println("Może zawierać tylko małe i duże litery, cyfry" +
                    " oraz spacje");
            System.out.println("(o ile nie znajdują się one na początku lub " +
                    "końcu nazwy).");
            System.out.println("Zbyt długa nazwa zostanie skrócona, a " +
                    "niepoprawne znaki usunięte.");
            System.out.println("Wpisz pustą linię, aby powrócić do " +
                    "poprzedniego menu.");
            name = Common.readLine();
            if (name.equals("")) {
                returnValue = "Anulowano dodawanie drużyny";
                break;
            } else {
                statement1.setString(1, name);
                ResultSet resultSet1 = statement1.executeQuery();
                if (resultSet1.isBeforeFirst()) {
                    info = "Istnieje już drużyna o podanej nazwie";
                    resultSet1.close();
                } else {
                    statement2.setString(1, name);
                    statement2.execute();
                    resultSet1.close();
                    db.commit();
                    returnValue = "Pomyślnie wstawiono drużynę " + name;
                    break;
                }
            }
        }
        statement1.close();
        statement2.close();
        return returnValue;
    }

    private String addPlayer(Connection db, String team)
            throws IOException, SQLException {
        String name, surname,
               SQL = "insert into zawodnicy values (default, ?, ?, ?)";
        PreparedStatement statement = db.prepareStatement(SQL);
        Common.clearView();
        System.out.println("Imię i nazwisko mogą zawierać od 1 do 30 znaków.");
        System.out.println("Mogą zawierać tylko małe i duże litery, cyfry, " +
                "oraz spacje");
        System.out.println("(o ile nie znajdują się one na początku lub " +
                "końcu nazwy).");
        System.out.println("Zbyt długie nazwy zostaną skrócone, a " +
                "niepoprawne znaki usunięte.");
        System.out.println("Wpisz pustą linię zamiast imienia lub nazwiska,");
        System.out.println("aby powrócić do poprzedniego menu.");
        System.out.println("Wpisz imię zawodnika:");
        name = Common.readLine();
        if (name.equals("")) {
            return "Anulowano dodawanie zawodnika";
        }
        System.out.println("Wpisz nazwisko zawodnika:");
        surname = Common.readLine();
        if (surname.equals("")) {
            return "Anulowano dodawanie zawodnika";
        }
        statement.setString(1, team);
        statement.setString(2, name);
        statement.setString(3, surname);
        statement.execute();
        db.commit();
        statement.close();
        return "Pomyślnie dodano zawodnika " + name + " " + surname +
                " do drużyny " + team;
    }

    private String editPlayerName(Connection db, int id)
            throws IOException, SQLException {
        String name, SQL = "update zawodnicy set imie = ? where id = ?";
        Common.clearView();
        System.out.println("Imię może zawierać od 1 do 30 znaków.");
        System.out.println("Może zawierać tylko małe i duże litery, cyfry, " +
                "oraz spacje");
        System.out.println("(o ile nie znajdują się one na początku lub " +
                "końcu nazwy).");
        System.out.println("Zbyt długie imię zostanie skrócone, a " +
                "niepoprawne znaki usunięte.");
        System.out.println("Wpisz pustą linię, aby powrócić do " +
                "poprzedniego menu bez edycji, lub");
        System.out.println("podaj nowe imię zawodnika:");
        name = Common.readLine();
        if (name.equals("")) {
            return "Anulowano zmianę imienia";
        }
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, name);
        statement.setInt(2, id);
        statement.execute();
        db.commit();
        statement.close();
        return "Zmieniono imię zawodnika";
    }

    private String editPlayerSurname(Connection db, int id)
            throws IOException, SQLException {
        String surname, SQL = "update zawodnicy set nazwisko = ? where id = ?";
        Common.clearView();
        System.out.println("Nazwisko może zawierać od 1 do 30 znaków.");
        System.out.println("Może zawierać tylko małe i duże litery, cyfry, " +
                "oraz spacje");
        System.out.println("(o ile nie znajdują się one na początku lub " +
                "końcu nazwy).");
        System.out.println("Zbyt długie nazwisko zostanie skrócone, a " +
                "niepoprawne znaki usunięte.");
        System.out.println("Wpisz pustą linię, aby powrócić do " +
                "poprzedniego menu bez edycji, lub");
        System.out.println("podaj nowe nazwisko zawodnika:");
        surname = Common.readLine();
        if (surname.equals("")) {
            return "Anulowano zmianę nazwiska";
        }
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, surname);
        statement.setInt(2, id);
        statement.execute();
        statement.close();
        db.commit();
        return "Zmieniono nazwisko zawodnika";
    }

    private String editPlayerTeam(Connection db, int id)
            throws IOException, SQLException {
        String team, info = null,
               SQL = "update zawodnicy set druzyna = ? where id = ?";
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            System.out.println("Lista wszystkich drużyn:");
            Common.displayAllTeams(db, false);
            System.out.println("Wpisz pustą linię, by powrócić bez zmian, lub");
            System.out.println("podaj nową drużynę zawodnika:");
            team = Common.readLine();
            if (team.equals("")) {
                return "Anulowano edycję drużyny, do której należy zawodnik";
            } else if (Common.teamExists(db, team)) {
                break;
            } else {
                info = "Nie istnieje drużyna o podanej nazwie";
            }
        }
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        statement.setInt(2, id);
        statement.execute();
        statement.close();
        db.commit();
        return "Zmieniono nazwisko zawodnika";
    }

    private boolean confirmDeletingPlayer() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy na pewno chcesz usunąć zawodnika?");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private String deletePlayer(Connection db, int id) throws SQLException {
        String SQL = "delete from zawodnicy where id = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        statement.execute();
        db.commit();
        statement.close();
        return "Zawodnik pomyślnie usunięty";
    }

    private String editPlayer(Connection db, int id)
            throws IOException, SQLException {
        String option, info = null;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            Common.displayPlayer(db, id);
            System.out.println("Wybierz jedną z opcji:");
            System.out.println("1 - edytuj imię");
            System.out.println("2 - edytuj nazwisko");
            System.out.println("3 - zmień drużynę");
            System.out.println("4 - usuń zawodnika i wróć do menu edycji " +
                    "drużyny");
            System.out.println("q - wróć do menu edycji drużyny");
            option = Common.readLine();
            switch (option) {
                case "1":
                    info = editPlayerName(db, id);
                    break;
                case "2":
                    info = editPlayerSurname(db, id);
                    break;
                case "3":
                    info = editPlayerTeam(db, id);
                    break;
                case "4":
                    if (confirmDeletingPlayer()) {
                        return deletePlayer(db, id);
                    } else break;
                case "q":
                    return "Zakończono edycję gracza";
            }
        }
    }

    private boolean confirmDeletingTeam() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy jesteś pewien, że chcesz usunąć drużynę?");
            System.out.println("Razem z nią usuniesz wszystkich prypisanych " +
                    "tej drużynie zawodników.");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private boolean confirmDeletingSquad() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy na pewno chcesz usunąć ten skład?");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private boolean confirmDeletingAllTeams() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy jesteś pewien, że chcesz usunąć wszystkie" +
                    " drużyny i wszystkich zawodników?");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private boolean confirmPlayingMatches() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Zgłoszenia zostaną zamknięte po wprowadzeniu " +
                    "pierwszego poprawnego wyniku.");
            System.out.println("Wpisz \"tak\", aby przejść do wprowadzania " +
                    "wyników lub \"nie\", aby powrócić.");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private boolean confirmDeletingMatch() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy jesteś pewien, że chcesz usunąć mecz?");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private String deleteTeam(Connection db, String team) throws SQLException {
        String SQL = "delete from druzyny where nazwa = ? cascade";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        statement.execute();
        db.commit();
        statement.close();
        return "Drużyna pomyślnie usunięta";
    }

    private String deleteAllTeams(Connection db) throws SQLException {
        String SQL = "truncate druzyny cascade";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.execute();
        db.commit();
        statement.close();
        return "Wyczyszczono bazę danych";
    }
    
    private void editTeam(Connection db, String team)
            throws IOException, SQLException {
        assert (team != null);
        String option, info = null;
        int playerID;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            Common.displayTeam(db, team, true);
            System.out.println("Wybierz jedną z opcji:");
            System.out.println("1 - dodaj zawodnika");
            System.out.println("2 - edytuj zawodnika");
            System.out.println("3 - usuń zawodnika");
            System.out.println("q - wróć do menu edycji drużyn");
            option = Common.readLine();
            switch (option) {
                case "1":
                    info = addPlayer(db, team);
                    break;
                case "2":
                    playerID = Common.selectPlayer(db, team);
                    if (playerID != -1) {
                        info = editPlayer(db, playerID);
                    }
                    break;
                case "3":
                    playerID = Common.selectPlayer(db, team);
                    if (playerID != -1 && confirmDeletingPlayer()) {
                        info = deletePlayer(db, playerID);
                    }
                    break;
                case "q":
                    return;
            }
        }
    }

    private boolean confirmTeams() throws IOException {
        String decision;
        while (true) {
            Common.clearView();
            System.out.println("Czy jesteś pewien? Jeśli przejdziesz dalej,");
            System.out.println("nie będziesz już mógł edytować drużyn.");
            System.out.println("Wpisz \"tak\" lub \"nie\".");
            decision = Common.readLine();
            if (decision.equals("tak")) {
                return true;
            } else if (decision.equals("nie")) {
                return false;
            }
        }
    }

    private boolean editTeams(Connection db) throws IOException, SQLException {
        String option, info = null, team;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            System.out.println("Menedżer drużyn - wybierz jedną z opcji:");
            System.out.println("1 - dodaj drużynę");
            System.out.println("2 - edytuj drużynę i jej zawodników");
            System.out.println("3 - zobacz wszystkie drużyny i ich zawodników");
            System.out.println("4 - usuń drużynę");
            System.out.println("5 - usuń wszystkie drużyny");
            System.out.println("6 - przejdź do menedżera turnieju");
            System.out.println("q - wyjdź z trybu organizatora");
            option = Common.readLine();
            switch (option) {
                case "1":
                    info = addTeam(db);
                    break;
                case "2":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        editTeam(db, team);
                        info = "Zakończono edycję drużyny " + team;
                    }
                    break;
                case "3":
                    Common.clearView();
                    Common.displayAllTeams(db, true);
                    Common.debug();
                    break;
                case "4":
                    team = Common.selectTeam(db);
                    if (team != null && confirmDeletingTeam()) {
                        info = deleteTeam(db, team);
                    }
                    break;
                case "5":
                    if (confirmDeletingAllTeams()) {
                        info = deleteAllTeams(db);
                    }
                    break;
                case "6":
                    if (confirmTeams()) {
                        return true;
                    } else break;
                case "q":
                    return false;
            }
        }
    }

    private String createSquad(Connection db, String team)
            throws IOException, SQLException {
        String SQL1 = "insert into sklady values (default, ?, ?, ?, ?, ?, ?, ?)"
                    + " returning id",
               SQL2 = "select weryfikuj_sklad(?)",
               SQL3 = "delete from sklady where id = ?";
        PreparedStatement statement;
        ResultSet resultSet;
        String info = null;
        int[] playerIDs;
        int squadSize = 6, squadID, errno;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            Common.displayPlayersFromTeam(db, team);
            System.out.println("Podaj identyfikatory zawodników, którzy mają " +
                    "należeć do tego składu");
            System.out.println("(w jednej linii, oddzielone spacją)");
            System.out.println("lub wpisz pustą linię, aby powrócić:");
            try {
                System.out.println("(w jednej linii, oddzielone spacją)");
                playerIDs = Common.readNumbersInLine(squadSize);
            } catch (ReadingNumbersException e) {
                if (e.getReason().equals("Empty line")) {
                    return "Anulowano tworzenie składu";
                } else {
                    info = e.getReason();
                    continue;
                }
            }
            statement = db.prepareStatement(SQL1);
            statement.setString(1, team);
            int nonexistentPlayer = -1;
            for (int i = 0; i < squadSize && nonexistentPlayer == -1; ++i) {
                if (!Common.playerExists(db, playerIDs[i]))
                    nonexistentPlayer = playerIDs[i];
            }
            if (nonexistentPlayer != -1) {
                info = "Nie istnieje zawodnik o numerze " + nonexistentPlayer;
                continue;
            }
            for (int i = 0; i < squadSize; ++i) {
                statement.setInt(i + 2, playerIDs[i]);
            }
            resultSet = statement.executeQuery();
            resultSet.next();
            squadID = resultSet.getInt(1);
            resultSet.close();
            statement.close();
            statement = db.prepareStatement(SQL2);
            statement.setInt(1, squadID);
            resultSet = statement.executeQuery();
            resultSet.next();
            errno = resultSet.getInt(1);
            resultSet.close();
            statement.close();
            if (errno != 0) {
                statement = db.prepareStatement(SQL3);
                statement.setInt(1, squadID);
                statement.execute();
                statement.close();
                if (errno == 7) {
                    info = "Skład zawierał powtarzające się numery";
                } else {
                    info = "Zawodnik " + playerIDs[errno - 1] + " nie należy " +
                           "do tej drużyny";
                }
            } else {
                db.commit();
                return "Pomyślnie wstawiono do bazy skład o id: " + squadID;
            }
            db.commit();
        }
    }

    private String deleteSquad(Connection db, int id)
            throws IOException, SQLException {
        String SQL = "delete from sklady where id = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        statement.execute();
        db.commit();
        statement.close();
        return "Skład pomyślnie usunięty";
    }

    private String deleteMatch(Connection db, int matchID) throws SQLException {
        String SQL1 = "delete from sety where idmeczu = ?",
               SQL2 = "delete from mecze where id = ?";
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                          statement2 = db.prepareStatement(SQL2);
        statement1.setInt(1, matchID);
        statement2.setInt(1, matchID);
        statement1.execute();
        statement2.execute();
        db.commit();
        statement1.close();
        statement2.close();
        return "Mecz pomyślnie usunięty";
    }

    // This function only does basic verification, just like checks in
    // create table sety
    private boolean verifyPoints(int sets, int[][] points) {
        for (int i = 0; i < sets; ++i) {
            if (points[0][i] < 0 || points[1][i] < 0 ||
                points[0][i] > 21 || points[1][i] > 21) {
                return false;
            }
            if (points[0][i] != 21 && points[1][i] != 21) {
                // Neither team won set
                return false;
            }
            if (points[0][i] == 21 && points[1][i] == 21) {
                // Both teams won set
                return false;
            }
        }
        return true;
    }

    private String planMatch(Connection db) throws IOException, SQLException {
        String info = null, option, returnString;
        String[] team = new String[2];
        int[] squad = {-1, -1};
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
                info = null;
            }
            System.out.println("--> Wybór drużyn");
            System.out.println("Za chwilę zostaniesz poproszony o wybranie ");
            System.out.println("dwóch drużyn, które będą ze sobą grać.");
            System.out.println("Pamiętaj, żeby wybrać dwie różne drużyny!");
            Common.debug();
            team[0] = Common.selectTeam(db);
            if (team[0] == null) {
                return "Przerwano dodawanie meczu: odmowa wyboru " +
                        "pierwszej drużyny";
            }
            team[1] = Common.selectTeam(db);
            if (team[1] == null) {
                return "Przerwano dodawanie meczu: odmowa wyboru " +
                        "drugiej drużyny";
            }
            if (team[0].equals(team[1])) {
                info = "Błąd danych: wybrano tą samą drużynę dwa razy";
            } else break;
        }
        for (int i = 0; i < 2; ++i) {
            while (true) {
                Common.clearView();
                if (info != null) {
                    System.out.println(info);
                    Common.printLine(info.length());
                }
                info = null;
                System.out.println(team[i] + ": wybierz jedną z opcji:");
                System.out.println("1 - utwórz nowy skład i graj tym składem");
                System.out.println("    (skład pozostanie w bazie nawet, " +
                        "jeśli mecz zostanie anulowany)");
                System.out.println("2 - wybierz utworzony wcześniej skład");
                System.out.println("q - przerwij tworzenie meczu i powróć " +
                        "do menedżera turnieju");
                option = Common.readLine();
                switch (option) {
                    case "1":
                        returnString = createSquad(db, team[i]);
                        if (returnString.substring(0, 9).equals("Anulowano")) {
                            info = returnString;
                            break;
                        } else {
                            //"Pomyślnie ... id: ".length = 40
                            try {
                                squad[i] = Integer.parseInt(returnString
                                        .substring(40));
                            } catch (NumberFormatException e) {
                                return "Przerwano dodawanie meczu: nieokreślony"
                                     + " błąd przy składzie drużyny " + team[i];
                            }
                        }
                        break;
                    case "2":
                        Common.clearView();
                        squad[i] = Common.selectSquad(db, team[i]);
                        if (squad[i] == -1) {
                            info = "Nie wybrano żadnego składu";
                        }
                        break;
                    case "q":
                        return "Przerwano dodawanie meczu: nie wybrano składu" +
                               " dla drużyny " + team[i];
                }
                if (squad[i] != -1) break;
            }
        }
        String SQL = "insert into mecze values (default, ?, ?)";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, squad[0]);
        statement.setInt(2, squad[1]);
        statement.execute();
        db.commit();
        return "Pomyślnie dodano mecz";
    }

    private String insertMatchResults(Connection db, int matchID)
            throws IOException, SQLException {
        String info = null, option;
        int[][] points = new int[2][];
        int sets;
        String SQL1 = "select * from mecze where id = ?",
               SQL2 = "select * from sklady where id = ?";
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                          statement2 = db.prepareStatement(SQL2);
        statement1.setInt(1, matchID);
        ResultSet resultSet1 = statement1.executeQuery(), resultSet2;
        resultSet1.next();
        int[] squad = {resultSet1.getInt("sklad1"),
                       resultSet1.getInt("sklad2")};
        resultSet1.close();
        statement1.close();
        String[] team = new String[2];
        for (int i = 0; i < 2; ++i) {
            statement2.setInt(1, squad[i]);
            resultSet2 = statement2.executeQuery();
            resultSet2.next();
            team[i] = resultSet2.getString("druzyna");
            resultSet2.close();
        }
        statement2.close();
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
                info = null;
            }
            System.out.println("Podaj wyniki meczu " + matchID +
                    " rozegranego między drużynami");
            System.out.println(team[0] + " i " + team[1] + ".");
            System.out.println("Najpierw podaj liczbę rozegranych setów,");
            System.out.println("następnie wyniki punktowe pierwszej drużyny,");
            System.out.println("a na końcu drugiej drużyny.");
            System.out.println("Pamiętaj o kolejności setów.");
            Common.debug();
            sets = -1;
            while (true) {
                Common.clearView();
                System.out.println("Podaj, ile setów zostało rozegranych " +
                        "(3, 4 lub 5)");
                System.out.println("lub wpisz pustą linię, aby powrócić:");
                option = Common.readLine();
                switch (option) {
                    case "":
                        return "Przerwano dodawanie meczu: " +
                                "odmowa podania wyniku (sety)";
                    case "3":
                        sets = 3;
                        break;
                    case "4":
                        sets = 4;
                        break;
                    case "5":
                        sets = 5;
                        break;
                }
                if (sets != -1) break;
            }
            for (int i = 0; i < 2; ++i) {
                while (true) {
                    Common.clearView();
                    if (info != null) {
                        System.out.println(info);
                        Common.printLine(info.length());
                    }
                    info = null;
                    System.out.println("Podaj wyniki punktowe kolejnych setów");
                    System.out.println("dla drużyny " + team[i]);
                    System.out.println("(w jednej linii, oddzielone spacją)");
                    System.out.println("lub wpisz pustą linię, aby powrócić:");
                    try {
                        points[i] = Common.readNumbersInLine(sets);
                    } catch (ReadingNumbersException e) {
                        if (e.getReason().equals("Empty line")) {
                            return "Przerwano dodawanie meczu: " +
                                    "odmowa podania wyniku (małe punkty)";
                        } else {
                            info = e.getReason();
                            continue;
                        }
                    }
                    break;
                }
            }
            if (!verifyPoints(sets, points)) {
                info = "Nieprawidłowe wyniki setów";
                continue;
            }
            String SQL3 = "insert into sety values (?, ?, ?, ?)",
                   SQL4 = "select weryfikuj_mecz(?)";
            PreparedStatement statement3 = db.prepareStatement(SQL3), statement4;
            for (int i = 0; i < sets; ++i) {
                statement3.setInt(1, matchID);
                statement3.setInt(2, i + 1);
                statement3.setInt(3, points[0][i]);
                statement3.setInt(4, points[1][i]);
                statement3.execute();
            }
            statement3.close();
            statement4 = db.prepareStatement(SQL4);
            statement4.setInt(1, matchID);
            ResultSet resultSet4 = statement4.executeQuery();
            resultSet4.next();
            boolean correct = resultSet4.getBoolean(1);
            resultSet4.close();
            // Function weryfikuj_mecz (in PL\SQL) will remove invalid results
            // and commit won't change data
            db.commit();
            statement4.close();
            if (correct) {
                return "Pomyślnie dodano wyniki";
            } else {
                info = "Nieprawidłowe wyniki setów";
            }
        }
    }

    private void deleteMatch(Connection db) throws IOException, SQLException {
        String option, info = null, team;
        int matchID, player;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            System.out.println("W celu usunięcia meczu wybierz jedną z opcji:");
            System.out.println("1 - wybierz spośród meczów, w których zagrał " +
                    "wybrany zawodnik");
            System.out.println("2 - wybierz spośród wszystkich meczów " +
                    "wybranej drużyny");
            System.out.println("q - powróć do menedżera turnieju");
            option = Common.readLine();
            switch (option) {
                case "1":
                    player = Common.selectPlayer(db);
                    if (player != -1) {
                        matchID = Common.selectMatch(db, player);
                        if (matchID != -1 && confirmDeletingMatch()) {
                            info = deleteMatch(db, matchID);
                        }
                    }
                    break;
                case "2":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        matchID = Common.selectMatch(db, team);
                        if (matchID != -1 && confirmDeletingMatch()) {
                            info = deleteMatch(db, matchID);
                        }
                    }
                    break;
                case "q":
                    return;
            }
        }
    }

    private void playMatches(Connection db) throws IOException, SQLException {
        String SQL1 = "select * from mecze order by id",
               SQL2 = "select * from sety where idmeczu = ?",
               info, option;
        int matchID;
        boolean noScroll = false;
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                          statement2 = db.prepareStatement(SQL2);
        ResultSet resultSet1 = statement1.executeQuery(), resultSet2;
        while (noScroll || resultSet1.next()) {
            noScroll = false;
            Common.clearView();
            matchID = resultSet1.getInt("id");
            statement2.setInt(1, matchID);
            resultSet2 = statement2.executeQuery();
            if (resultSet2.isBeforeFirst()) {
                // we skip matches where we already have results
                resultSet2.close();
                continue;
            }
            info = insertMatchResults(db, matchID);
            if (info.equals("Pomyślnie dodano wyniki")) {
                Common.clearView();
                System.out.println(info);
                Common.printLine(info.length());
                Common.debug();
            } else {
                while (true) {
                    Common.clearView();
                    System.out.println(info);
                    Common.printLine(info.length());
                    System.out.println("Wybierz, co chcesz zrobić:");
                    System.out.println("1 - dodaj ponownie wynik do tego meczu");
                    System.out.println("2 - przejdź do następnego meczu");
                    System.out.println("q - wyloguj się");
                    option = Common.readLine();
                    if (option.equals("1") || option.equals("2") || option.equals("q")) {
                        break;
                    }
                }
                if (option.equals("q")) {
                    resultSet2.close();
                    resultSet1.close();
                    statement1.close();
                    statement2.close();
                    return;
                }
                if (option.equals("1")) {
                    noScroll = true;
                }
            }
            resultSet2.close();
        }
        resultSet1.close();
        statement1.close();
        statement2.close();
        Common.clearView();
        System.out.println("Wprowadzono wszystkie mecze");
        System.out.println("Aby oglądnąć wyniki, przejdź do trybu kibica.");
        Common.debug();
    }

    private void beginTournament(Connection db)
            throws IOException, SQLException {
        if (Common.matchesHaveStarted(db)) {
            playMatches(db);
            return;
        }

        String option, info = null, team,
               SQL1 = "select zakoncz_zgloszenia()",
               SQL2 = "select rozegraj_mecze()";
        int player;
        CallableStatement statement = db.prepareCall(SQL1);
        statement.execute();
        db.commit();
        statement.close();
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            System.out.println("Menedżer turnieju - wybierz jedną z opcji:");
            System.out.println("1 - dodaj skład");
            System.out.println("2 - zobacz składy");
            System.out.println("3 - usuń składy");
            System.out.println("4 - zaplanuj mecz");
            System.out.println("5 - zobacz mecze wybranego zawodnika");
            System.out.println("6 - zobacz mecze wybranej drużyny");
            System.out.println("7 - usuń mecz");
            System.out.println("8 - rozegraj zaplanowane mecze");
            System.out.println("q - wyjdź z trybu organizatora");
            option = Common.readLine();
            switch (option) {
                case "1":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        info = createSquad(db, team);
                    }
                    break;
                case "2":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        Common.clearView();
                        Common.displayAllSquads(db, team);
                        Common.debug();
                    }
                    break;
                case "3":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        Common.clearView();
                        int squad = Common.selectSquad(db, team);
                        if (squad != -1) {
                            if (Common.squadPlayed(db, squad)) {
                                info = "Nie można usunąć składu, który już " +
                                       "zagrał mecz";
                            } else if (confirmDeletingSquad()) {
                                info = deleteSquad(db, squad);
                            }
                        }
                    }
                    break;
                case "4":
                    info = planMatch(db);
                    break;
                case "5":
                    player = Common.selectPlayer(db);
                    if (player != -1) {
                        Common.seePlayer(db, player);
                    }
                    break;
                case "6":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        Common.seeMatches(db, team);
                    }
                    break;
                case "7":
                    deleteMatch(db);
                    break;
                case "8":
                    if (confirmPlayingMatches()) {
                        statement = db.prepareCall(SQL2);
                        statement.execute();
                        db.commit();
                        statement.close();
                        playMatches(db);
                        return;
                    }
                    break;
                case "q":
                    return;
            }
        }
    }

    void run() throws IOException, SQLException, ClassNotFoundException {
        Common.clearView();
        Connection db = Common.connection();
        if (login(db)) {
            boolean playingGames = true;
            if (chooseMode(db).equals("editTeams")) {
                playingGames = editTeams(db);
            }
            if (playingGames) {
                beginTournament(db);
            }
            db.close();
        }
    }

}
