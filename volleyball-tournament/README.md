# Volleyball tournament

Assignment from Databases course.
Short description of problem: Volleyball teams have players and they can select squads. One squad consists of exactly 6 players, player can belong to only one team, but many squads declared by this team. In volleyball match all sets are played up to 21 points (without advantages), otherwise standard rules. Application is in text mode and provides two modules:
* Organizer creates and manages teams, players and squads, creates matches and inserts results. To use this panel, organizer needs to login (sadly, passwords in database are in plain text).
* Fan can view results of matches and doesn't need to log in.

Notes:
1. File `diagram.png` contains entity diagram. To create it, use `turniej.sql`. However, this script doesn't add admin credentials to database, you have to do it on your own. I used PostgreSQL.
2. App's entry point is Turniej.java.
3. If you want to try it out in your own database, coonection is made in `connection` function in Common.java. Change URL, username and password.
