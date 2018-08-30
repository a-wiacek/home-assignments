package turniej;

import java.io.IOException;
import java.sql.*;

public class Turniej {

    private static String selectMode() throws IOException {
        String mode;
        while (true) {
            System.out.println("Wpisz jedną z następujących opcji:");
            System.out.println("org - panel zarządzania organizatora");
            System.out.println("kibic - panel kibica");
            System.out.println("quit - wyjście z programu");
            mode = Common.readLine();
            if (mode.equals("org") || mode.equals("kibic") || mode.equals("quit")) {
                return mode;
            } else {
                Common.clearView();
            }
        }
    }

    public static void main (String args[]) throws ClassNotFoundException,
            SQLException, IOException {
        Common.clearView();
        System.out.println("Witaj w programie Turniej siatkarski!");
        String mode = "";
        while (!mode.equals("quit")) {
            mode = selectMode();
            switch (mode) {
                case "org":
                    new Organizator().run();
                    Common.clearView();
                    System.out.println("Pomyślnie wylogowałeś się z aplikacji");
                    break;
                case "kibic":
                    new Kibic().run();
                    Common.clearView();
                    System.out.println("Pomyślnie wyszedłeś z trybu kibica");
                    break;
                default:
                    Common.clearView();
                    break;
            }
        }
    }
}