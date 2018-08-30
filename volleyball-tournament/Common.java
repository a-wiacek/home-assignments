package turniej;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.*;

class Common {

    static Connection connection() throws ClassNotFoundException,
            SQLException {
        Class.forName("org.postgresql.Driver");
        // GitHub edit: I won't show credentials here
        String url = "URL";
        String user = "USER";
        String passwd = "PASSWORD";
        Connection db = DriverManager.getConnection(url, user, passwd);
        db.setAutoCommit(false);
        return db;
    }

    // Note: readLine() will truncate strings longer than maxSize signs and
    // remove all nonalphanumeric signs, leading and trailing whitespaces.
    // This is not to prevent SQL injections (PreparedStatement does its job),
    // but to prevent from creating very similar players
    // (in fact indistinguishable from console), such as
    // "Jan Kowalski", "Jan Kowalski ", "Jan Kowalski  " and so on.
    // Default parameter is 30.
    static String readLine(int maxSize) throws IOException {
        BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
        String line = r.readLine();
        if (line == null) {
            return "";
        } else {
            return line.substring(0, Math.min(line.length(), maxSize))
                    .replaceAll("/[^A-Za-z0-9]/", "").trim();
        }
    }

    static String readLine() throws IOException {
        return readLine(30);
    }

    static String readPassword() throws IOException {
        char[] password = System.console().readPassword();
        if (password == null) {
            return "";
        } else {
            return String.valueOf(password);
        }
    }

    static void debug() throws IOException {
        System.out.println("Naciśnij ENTER, aby kontynuować:");
        System.in.read();
    }

    static void clearView() {
        System.out.print("\033[H\033[2J");
    }

    static void printLine(int length) {
        // Console line has propably 80 signs
        int actual_length = Math.min(length, 80);
        for (int i = 0; i < length; ++i) {
            System.out.print('-');
        }
        if (actual_length < 80) {
            System.out.print('\n');
        }
    }

    static void printLine() {
        printLine(30);
    }

    static int[] readNumbersInLine(int expectedSize)
            throws IOException, ReadingNumbersException {
        // Increase input just in case
        String line = readLine(50);
        if (line.equals("")) {
            throw new ReadingNumbersException("Empty line");
        }
        String[] readStrings = line.split("\\s+");
        if (readStrings.length != expectedSize) {
            throw new ReadingNumbersException("Niewłaściwa liczba argumentów:" +
                    " wczytano " + readStrings.length + ", oczekiwano " +
                    expectedSize);
        }
        int[] result = new int[expectedSize];
        for (int i = 0; i < expectedSize; ++i) {
            try {
                result[i] = Integer.parseInt(readStrings[i]);
            } catch (NumberFormatException e) {
                throw new ReadingNumbersException("Niewłaściwy argument nr "
                        + (i + 1) + " (nie jest liczbą)");
            }
        }
        return result;
    }

    // Procedures stored in Common are accesible to both Organizator and Kibic
    // and they don't change database

    static boolean tournamentHasBegun(Connection db) throws SQLException {
        String SQL = "select count(*) from trwa_turniej where trwa = 0";
        PreparedStatement statement = db.prepareStatement(SQL);
        ResultSet resultSet = statement.executeQuery();
        resultSet.next();
        int count = resultSet.getInt(1);
        resultSet.close();
        statement.close();
        return count == 0;
    }

    static boolean matchesHaveStarted(Connection db) throws SQLException {
        String SQL = "select count(*) from trwa_turniej where trwa = 2";
        PreparedStatement statement = db.prepareStatement(SQL);
        ResultSet resultSet = statement.executeQuery();
        resultSet.next();
        int count = resultSet.getInt(1);
        resultSet.close();
        statement.close();
        return count == 1;
    }

    static boolean teamExists(Connection db, String team)
            throws SQLException {
        String SQL = "select * from druzyny where nazwa = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        ResultSet resultSet = statement.executeQuery();
        boolean answer = resultSet.next();
        resultSet.close();
        statement.close();
        return answer;
    }

    static boolean playerExists(Connection db, int id)
            throws SQLException {
        String SQL = "select * from zawodnicy where id = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        ResultSet resultSet = statement.executeQuery();
        boolean answer = resultSet.next();
        resultSet.close();
        statement.close();
        return answer;
    }

    static boolean playerBelongs(Connection db, int id, String team)
            throws SQLException {
        String SQL = "select * from zawodnicy where id = ? and druzyna = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        statement.setString(2, team);
        ResultSet resultSet = statement.executeQuery();
        boolean answer = resultSet.next();
        resultSet.close();
        statement.close();
        return answer;
    }

    static boolean playerBelongs(Connection db, int playerID, int squadID)
            throws SQLException {
        String SQL = "select * from sklady where id = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, squadID);
        ResultSet resultSet = statement.executeQuery();
        if (!resultSet.next()) {
            resultSet.close();
            statement.close();
            return false;
        }
        for (int i = 1; i <= 6; ++i) {
            if (resultSet.getInt("zawodnik" + i) == playerID) {
                resultSet.close();
                statement.close();
                return true;
            }
        }
        resultSet.close();
        statement.close();
        return false;
    }

    static boolean squadExistsAndBelongs(Connection db, int id, String team)
            throws SQLException {
        String SQL = "select * from sklady where id = ? and druzyna = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        statement.setString(2, team);
        ResultSet resultSet = statement.executeQuery();
        boolean answer = resultSet.next();
        resultSet.close();
        statement.close();
        return answer;
    }

    static boolean squadPlayed(Connection db, int id)
            throws SQLException {
        String SQL = "select * from mecze where sklad1 = ? or sklad2 = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        statement.setInt(2, id);
        ResultSet resultSet = statement.executeQuery();
        boolean answer = resultSet.next();
        resultSet.close();
        statement.close();
        return answer;
    }

    static boolean matchWasPlayedBy(Connection db, int matchID, String team)
            throws SQLException {
        String SQL1 = "select * from mecze where id = ?",
                SQL2 = "select druzyna from sklady where id = ?";
        int[] squad = new int[2];
        boolean[] answer = new boolean[2];
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                statement2 = db.prepareStatement(SQL2);
        statement1.setInt(1, matchID);
        ResultSet resultSet = statement1.executeQuery();
        if (!resultSet.next()) {
            return false;
        }
        squad[0] = resultSet.getInt("sklad1");
        squad[1] = resultSet.getInt("sklad2");
        resultSet.close();
        statement1.close();
        for (int i = 0; i < 2; ++i) {
            statement2.setInt(1, squad[i]);
            resultSet = statement2.executeQuery();
            if (resultSet.next()) {
                answer[i] = resultSet.getString(1).equals(team);
            } else {
                answer[i] = false;
            }
            resultSet.close();
        }
        statement2.close();
        return answer[0] || answer[1];
    }

    static boolean matchWasPlayedBy(Connection db, int matchID, int playerID)
            throws SQLException {
        String SQL1 = "select * from mecze where id = ?";
        int[] squad = new int[2];
        PreparedStatement statement = db.prepareStatement(SQL1);
        statement.setInt(1, matchID);
        ResultSet resultSet = statement.executeQuery();
        if (!resultSet.next()) {
            return false;
        }
        squad[0] = resultSet.getInt("sklad1");
        squad[1] = resultSet.getInt("sklad2");
        return playerBelongs(db, playerID, squad[0])
            || playerBelongs(db, playerID, squad[1]);
    }

    static String selectTeam(Connection db)
            throws IOException, SQLException {
        String team, info = null;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayAllTeams(db, false);
            System.out.println("Wpisz nazwę drużyny, którą chcesz wybrać, lub" +
                    " wpisz pustą linię, aby powrócić:");
            team = readLine();
            if (team.equals("")) {
                return null;
            } else {
                if (teamExists(db, team)) {
                    break;
                } else {
                    info = "Niepoprawna nazwa drużyny";
                }
            }
        }
        return team;
    }

    static int selectPlayer(Connection db, String team)
            throws IOException, SQLException {
        String info = null, playerString;
        int player;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayPlayersFromTeam(db, team);
            System.out.println("Wpisz ID zawodnika z listy lub wpisz pustą " +
                    "linię, aby powrócić:");
            playerString = readLine();
            if (playerString.equals("")) {
                return -1;
            }
            try {
                player = Integer.parseInt(playerString);
            } catch (NumberFormatException e) {
                info = "Nieprawidłowy format: wpisz liczbę";
                continue;
            }
            if (playerBelongs(db, player, team)) {
                return player;
            } else {
                info = "W tej drużynie nie ma zawodnika o podanym numerze";
            }
        }
    }

    static int selectPlayer(Connection db) throws IOException, SQLException {
        String info = null, playerString;
        int player;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayAllPlayers(db);
            System.out.println("Wybierz ID zawodnika z listy lub wpisz pustą " +
                    "linię, aby powrócić:");
            playerString = readLine();
            if (playerString.equals("")) {
                return -1;
            }
            try {
                player = Integer.parseInt(playerString);
            } catch (NumberFormatException e) {
                info = "Nieprawidłowy format: wpisz liczbę";
                continue;
            }
            if (playerExists(db, player)) {
                return player;
            } else {
                info = "Nie istnieje zawodnik o podanym numerze";
            }
        }
    }

    static int selectSquad(Connection db, String team)
            throws IOException, SQLException {
        String info = null, playerString;
        int squad;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayAllSquads(db, team);
            System.out.println("Wybierz ID składu z listy lub wpisz pustą " +
                    "linię, aby powrócić:");
            playerString = readLine();
            if (playerString.equals("")) {
                return -1;
            }
            try {
                squad = Integer.parseInt(playerString);
            } catch (NumberFormatException e) {
                info = "Nieprawidłowy format: wpisz liczbę";
                continue;
            }
            if (squadExistsAndBelongs(db, squad, team)) {
                return squad;
            } else {
                info = "Drużyna nie wystawia składu o podanym numerze";
            }
        }
    }

    static int selectMatch(Connection db, String team)
            throws IOException, SQLException {
        String info = null, matchString;
        int matchID;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayMatches(db, team);
            System.out.println("Wybierz ID meczu z listy lub wpisz pustą " +
                    "linię, aby powrócić:");
            matchString = readLine();
            if (matchString.equals("")) {
                return -1;
            }
            try {
                matchID = Integer.parseInt(matchString);
            } catch (NumberFormatException e) {
                info = "Nieprawidłowy format: wpisz liczbę";
                continue;
            }
            if (matchWasPlayedBy(db, matchID, team)) {
                return matchID;
            } else {
                info = "Ta drużyna nie grała w meczu " + matchID +
                        " lub podany mecz nie istnieje";
            }
        }
    }

    static int selectMatch(Connection db, int playerID)
            throws IOException, SQLException {
        String info = null, matchString;
        int matchID;
        while (true) {
            clearView();
            if (info != null) {
                System.out.println(info);
                printLine(info.length());
            }
            displayMatches(db, playerID);
            System.out.println("Wybierz ID meczu z listy lub wpisz pustą " +
                    "linię, aby powrócić:");
            matchString = readLine();
            if (matchString.equals("")) {
                return -1;
            }
            try {
                matchID = Integer.parseInt(matchString);
            } catch (NumberFormatException e) {
                info = "Nieprawidłowy format: wpisz liczbę";
                continue;
            }
            if (matchWasPlayedBy(db, matchID, playerID)) {
                return matchID;
            } else {
                info = "Ten zawodnik nie grał w meczu " + matchID +
                        " lub podany mecz nie istnieje";
            }
        }
    }

    static void displayPlayersFromTeam(Connection db, String team)
            throws SQLException {
        String SQL = "select * from zawodnicy where druzyna = ? " +
                     "order by nazwisko",
               name, surname;
        int playerID;
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        ResultSet resultSet = statement.executeQuery();
        System.out.println("Zawodnicy (imię, nazwisko, ID w bazie danych):");
        if (!resultSet.isBeforeFirst()) {
            System.out.println("    brak");
        } else while (resultSet.next()) {
            name = resultSet.getString("imie");
            surname = resultSet.getString("nazwisko");
            playerID = resultSet.getInt("id");
            System.out.println("    " + name + " " + surname + " " + playerID);
        }
        resultSet.close();
        statement.close();
    }

    static void displayAllPlayers(Connection db) throws SQLException {
        String SQL = "select * from zawodnicy order by nazwisko",
               name, surname, team;
        PreparedStatement statement = db.prepareStatement(SQL);
        ResultSet resultSet = statement.executeQuery();
        int playerID;
        System.out.println("Zawodnicy (imię, nazwisko, drużyna, ID w bazie " +
                "danych):");
        if (!resultSet.isBeforeFirst()) {
            System.out.println("    brak");
        } else while (resultSet.next()) {
            name = resultSet.getString("imie");
            surname = resultSet.getString("nazwisko");
            team = resultSet.getString("druzyna");
            playerID = resultSet.getInt("id");
            System.out.println("    " + name + " " + surname + " " +
                    team + " " + playerID);
        }
        resultSet.close();
        statement.close();
    }

    static void displaySquad(Connection db, int id) throws SQLException {
        String SQL1 = "select * from sklady where id = ?",
               SQL2 = "select * from zawodnicy where id = ?",
               name, surname;
        PreparedStatement statement1 = db.prepareStatement(SQL1);
        PreparedStatement statement2 = db.prepareStatement(SQL2);
        statement1.setInt(1, id);
        ResultSet resultSet1 = statement1.executeQuery(), resultSet2;
        resultSet1.next();
        System.out.println("Id składu: " + id);
        for (int i = 1; i <= 6; ++i) {
            statement2.setInt(1, resultSet1.getInt("zawodnik" + i));
            resultSet2 = statement2.executeQuery();
            resultSet2.next();
            name = resultSet2.getString("imie");
            surname = resultSet2.getString("nazwisko");
            System.out.println("  " + name + " " + surname);
            resultSet2.close();
        }
        resultSet1.close();
        statement1.close();
        statement2.close();
    }

    static void displayAllSquads(Connection db, String team)
            throws IOException, SQLException {
        String SQL = "select id from sklady where druzyna = ? order by id";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        ResultSet resultSet = statement.executeQuery();
        System.out.println("Składy drużyny " + team + ":");
        if (!resultSet.isBeforeFirst()) {
            System.out.println("  brak");
        } else while (resultSet.next()) {
            displaySquad(db, resultSet.getInt("id"));
        }
        resultSet.close();
        statement.close();
    }

    static void displayTeam(Connection db, String team, boolean displayPlayers)
            throws SQLException {
        String SQL = "select * from zawodnicy where druzyna = ? " +
                     "order by nazwisko";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        ResultSet resultSet = statement.executeQuery();
        System.out.println("Drużyna " + team);
        if (displayPlayers) {
            displayPlayersFromTeam(db, team);
        }
        resultSet.close();
        statement.close();
    }

    static void displayAllTeams(Connection db, boolean displayPlayers)
            throws SQLException, IOException {
        String SQL1 = "select * from druzyny order by nazwa";
        PreparedStatement statement = db.prepareStatement(SQL1);
        ResultSet resultSet = statement.executeQuery();
        while (resultSet.next()) {
            String team = resultSet.getString(1);
            System.out.println(team);
            if (displayPlayers) {
                displayPlayersFromTeam(db, team);
            }
        }
        resultSet.close();
        statement.close();
    }

    static void displayPlayer(Connection db, int id) throws SQLException {
        assert (id != -1);
        String SQL = "select * from zawodnicy where id = ?";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setInt(1, id);
        ResultSet resultSet = statement.executeQuery();
        resultSet.next();
        String name = resultSet.getString("imie");
        String surname = resultSet.getString("nazwisko");
        String team = resultSet.getString("druzyna");
        System.out.println("Imię: " + name);
        System.out.println("Nazwisko: " + surname);
        System.out.println("Drużyna: " + team);
        resultSet.close();
        statement.close();
    }

    static void displayMatch(Connection db, int id) throws SQLException {
        String SQL1 = "select * from mecze where id = ?",
               SQL2 = "select * from sklady where id = ?",
               SQL3 = "select * from sety where idmeczu = ? order by nrsetu",
               team1, team2;
        PreparedStatement statement1 = db.prepareStatement(SQL1),
                          statement2 = db.prepareStatement(SQL2),
                          statement3 = db.prepareStatement(SQL3);
        statement1.setInt(1, id);
        ResultSet resultSet1 = statement1.executeQuery(), resultSet2;
        resultSet1.next();
        statement2.setInt(1, resultSet1.getInt("sklad1"));
        resultSet2 = statement2.executeQuery();
        resultSet2.next();
        team1 = resultSet2.getString("druzyna");
        resultSet2.close();
        statement2.setInt(1, resultSet1.getInt("sklad2"));
        resultSet2 = statement2.executeQuery();
        resultSet2.next();
        team2 = resultSet2.getString("druzyna");
        resultSet2.close();
        System.out.println("Mecz o numerze " + id + " między " +
                "drużynami " + team1 + " i " + team2);
        Common.printLine();
        System.out.println("Skład drużyny " + team1 + ":");
        Common.displaySquad(db, resultSet1.getInt("sklad1"));
        Common.printLine();
        System.out.println("Skład drużyny " + team2 + ":");
        Common.displaySquad(db, resultSet1.getInt("sklad2"));
        Common.printLine();
        resultSet1.close();
        statement1.close();
        statement2.close();
        statement3.setInt(1, id);
        ResultSet resultSet3 = statement3.executeQuery();
        int sety1 = 0, sety2 = 0, punkty1, punkty2, nrSetu = 0;
        if (!resultSet3.isBeforeFirst()) {
            System.out.println("Nie rozegrano jeszcze tego meczu.");
            resultSet3.close();
            return;
        }
        System.out.println("Wyniki:");
        // "Set n: 21 21".length = 12
        Common.printLine(12);
        while (resultSet3.next()) {
            punkty1 = resultSet3.getInt("wynik1");
            punkty2 = resultSet3.getInt("wynik2");
            if (punkty1 == 21) ++sety1;
            if (punkty2 == 21) ++sety2;
            ++nrSetu;
            System.out.print("Set " + nrSetu + ": ");
            if (punkty1 < 10) System.out.print(" ");
            System.out.print(punkty1 + " ");
            if (punkty2 < 10) System.out.print(" ");
            System.out.print(punkty2 + "\n");
        }
        Common.printLine(12);
        System.out.println("Wynik:  " + sety1 + "  " + sety2);
        if (sety1 == 3) {
            System.out.println("Zwycięzcy: " + team1);
        } else {
            System.out.println("Zwycięzcy: " + team2);
        }
        System.out.println();
        resultSet3.close();
    }

    static void displayMatches(Connection db, String team) throws SQLException {
        // For team display all matches where squads of that team played
        String SQL = "select mecze.id from mecze join (select id from sklady " +
                "where druzyna = ?) mojesklady on sklad1 = mojesklady.id " +
                "or sklad2 = mojesklady.id order by mecze.id";
        PreparedStatement statement = db.prepareStatement(SQL);
        statement.setString(1, team);
        ResultSet resultSet = statement.executeQuery();
        if (!resultSet.isBeforeFirst()) {
            System.out.println("  brak");
        } else while (resultSet.next()) {
            displayMatch(db, resultSet.getInt(1));
        }
    }

    static void displayMatches(Connection db, int id) throws SQLException {
        // Display all matchers where player id played
        String SQL = "select mecze.id from mecze join (select id from sklady " +
                "where zawodnik1 = ? or zawodnik2 = ? or zawodnik3 = ? " +
                "or zawodnik4 = ? or zawodnik5 = ? or zawodnik6 = ?) mojesklady"
                + " on sklad1 = mojesklady.id or sklad2 = mojesklady.id order by mecze.id";
        PreparedStatement statement = db.prepareStatement(SQL);
        for (int i = 1; i <= 6; ++i) {
            statement.setInt(i, id);
        }
        ResultSet resultSet = statement.executeQuery();
        if (!resultSet.isBeforeFirst()) {
            System.out.println("Zawodnik nie rozegrał jeszcze żadnego meczu");
        } else {
            System.out.println("Zawodnik zagrał w następujących meczach:");
            while (resultSet.next()) {
                Common.displayMatch(db, resultSet.getInt(1));
            }
        }
        resultSet.close();
        statement.close();
    }

    static void seePlayer(Connection db, int id)
            throws IOException, SQLException {
        Common.clearView();
        Common.displayPlayer(db, id);
        Common.displayMatches(db, id);
        Common.debug();
    }

    static void seeMatches(Connection db, String team)
            throws SQLException, IOException {
        Common.clearView();
        System.out.println("Mecze drużyny " + team + ":");
        Common.displayMatches(db, team);
        Common.debug();
    }
}
