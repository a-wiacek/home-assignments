package turniej;

import java.io.IOException;
import java.sql.*;

class Kibic {

    private void seeTeam(Connection db, String team)
            throws IOException, SQLException {
        String option;
        while (true) {
            Common.clearView();
            Common.displayTeam(db, team, true);
            System.out.println("Wybierz jedną z opcji:");
            System.out.println("1 - zobacz zawodnika");
            System.out.println("2 - zobacz mecze");
            System.out.println("q - wróć do menedżera kibica");
            option = Common.readLine();
            switch (option) {
                case "1":
                    seePlayer(db, team);
                    break;
                case "2":
                    Common.seeMatches(db, team);
                    break;
                case "q":
                    return;
            }
        }
    }

    private void seePlayer(Connection db, String team)
            throws IOException, SQLException {
        int id = Common.selectPlayer(db, team);
        if (id != -1) {
            Common.seePlayer(db, id);
        }
    }

    void run() throws IOException, SQLException, ClassNotFoundException {
        Connection db = Common.connection();
        Common.clearView();
        String option, info = null, team;
        int player;
        while (true) {
            Common.clearView();
            if (info != null) {
                System.out.println(info);
                Common.printLine(info.length());
            }
            info = null;
            System.out.println("Menedżer kibica - wybierz jedną z opcji:");
            System.out.println("1 - zobacz drużynę");
            System.out.println("2 - zobacz mecze wybranego zawodnika");
            System.out.println("3 - zobacz mecze wybranej drużyny");
            System.out.println("q - wyjdź z trybu kibica");
            option = Common.readLine();
            switch (option) {
                case "1":
                    team = Common.selectTeam(db);
                    if (team != null) {
                        seeTeam(db, team);
                    }
                    break;
                case "2":
                    player = Common.selectPlayer(db);
                    if (player != -1) {
                        Common.seePlayer(db, player);
                    }
                    break;
                case "3":
                    if (Common.tournamentHasBegun(db)) {
                        team = Common.selectTeam(db);
                        if (team != null) {
                            Common.seeMatches(db, team);
                        }
                    } else {
                        info = "Turniej się jeszcze nie rozpoczął";
                    }
                    break;
                case "q":
                    db.close();
                    return;
            }
        }
    }
}
