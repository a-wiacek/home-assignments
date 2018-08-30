create table druzyny (
  nazwa varchar(30),
  primary key (nazwa)
);

create table zawodnicy (
  id serial unique,
  druzyna varchar(30) references druzyny not null,
  imie varchar(30) not null,
  nazwisko varchar(30) not null,
  primary key (id)
);

create table sklady (
  id serial unique,
  druzyna varchar(30) references druzyny not null,
  zawodnik1 integer references zawodnicy not null,
  zawodnik2 integer references zawodnicy not null,
  zawodnik3 integer references zawodnicy not null,
  zawodnik4 integer references zawodnicy not null,
  zawodnik5 integer references zawodnicy not null,
  zawodnik6 integer references zawodnicy not null,
  primary key (id)
);

create table mecze (
  id serial unique,
  sklad1 integer references sklady not null,
  sklad2 integer references sklady not null,
  primary key (id)
);

create table sety (
  idmeczu integer references mecze not null,
  nrsetu numeric(1) not null,
  wynik1 numeric(2) not null,
  wynik2 numeric(2) not null,
  primary key (idmeczu, nrsetu),
  check (nrsetu >= 1 and nrsetu <= 5),
  check (wynik1 >= 0 and wynik2 >= 0),
  check ((wynik1 = 21 and wynik2 < 21) or (wynik1 < 21 and wynik2 = 21))
);

create table organizatorzy (
  login varchar(30) unique not null,
  haslo varchar(30) not null
);
 
-- Login i hasło wstawiam do bazy w innym miejscu.
-- Kto ma wiedzieć, ten wie (jakimi danymi się zalogować).

create table trwa_turniej (
  trwa numeric(1) unique not null
);

-- Tabela trwa_turniej pełni rolę "zmiennej globalnej".
-- Odpowiada ona za kilka rzeczy:
-- 1. Gdy jest ustawiona na 0, wolno edytować tabele druzyny i zawodnicy 
-- 2. Gdy jest ustawiona na 1, wolno edytować tabele sklady i mecze
-- 3. Gdy jest ustawiona na 2, wolno edytować tylko tabelę sety
-- 4. Nie wolno jej tak sobie zmienić; w górę można ją zmieniać o 1, a w dół
--    tylko do zera, kasując przy okazji tabele sklady, mecze i sety 
-- 5. Przed przypadkową zmianą zmiennej chroni wyzwalacz, który jest tymczasowo
--    wyłączany w funkcjach

insert into trwa_turniej values (0);

create or replace function nic() returns trigger as $$
begin
  return null;
end;
$$ language plpgsql;

create or replace function wprowadzanie_druzyn() returns trigger as $$
begin
  if exists (select * from trwa_turniej where trwa = 0)
  then 
    if tg_op = 'INSERT' or tg_op = 'UPDATE'
    then return new;
    else return old;
    end if;
  else return null;
  end if;
end;
$$ language plpgsql;

create or replace function wprowadzanie_meczy() returns trigger as $$
begin
  if exists (select * from trwa_turniej where trwa = 1)
  then 
    if tg_op = 'INSERT' or tg_op = 'UPDATE'
    then return new;
    else return old;
    end if;
  else return null;
  end if;
end;
$$ language plpgsql;

create or replace function granie_meczy() returns trigger as $$
begin
  if exists (select * from trwa_turniej where trwa = 2)
  then 
    if tg_op = 'INSERT' or tg_op = 'UPDATE'
    then return new;
    else return old;
    end if;
  else return null;
  end if;
end;
$$ language plpgsql;

create trigger ochrona_trwa
  before insert or update or delete on trwa_turniej
  for each row execute procedure nic();

create trigger ochrona_skladow
  before insert or update or delete on sklady
  for each row execute procedure wprowadzanie_meczy();

create trigger ochrona_meczy
  before insert or update or delete on mecze
  for each row execute procedure wprowadzanie_meczy();

create trigger ochrona_setow
  before insert or update or delete on sety
  for each row execute procedure granie_meczy();

create trigger ochrona_druzyn
  before insert or update or delete on druzyny
  for each row execute procedure wprowadzanie_druzyn();

create trigger ochrona_zawodnikow
  before insert or update or delete on zawodnicy
  for each row execute procedure wprowadzanie_druzyn();

create or replace function zakoncz_zgloszenia() returns void as $$
begin
  alter table trwa_turniej disable trigger ochrona_trwa;
  update trwa_turniej set trwa = 1 where trwa = 0;
  alter table trwa_turniej enable trigger ochrona_trwa;
end;
$$ language plpgsql;

create or replace function rozegraj_mecze() returns void as $$
begin
  alter table trwa_turniej disable trigger ochrona_trwa;
  update trwa_turniej set trwa = 2 where trwa = 1;
  alter table trwa_turniej enable trigger ochrona_trwa;
end;
$$ language plpgsql;

create or replace function rozpocznij_nowy_turniej() returns void as $$
begin
  alter table trwa_turniej disable trigger ochrona_trwa;
  truncate sety cascade;
  update trwa_turniej set trwa = 1 where trwa = 2;
  truncate sklady cascade;
  update trwa_turniej set trwa = 0 where trwa = 1;
  alter table trwa_turniej enable trigger ochrona_trwa;
end;
$$ language plpgsql;

create or replace function weryfikuj_sklad(integer) returns integer as $$
declare
  sklad sklady%rowtype;
  d1 varchar(30);
  d2 varchar(30);
  d3 varchar(30);
  d4 varchar(30);
  d5 varchar(30);
  d6 varchar(30);
begin
  select * from sklady where id = $1 into sklad;
  if sklad.zawodnik1 = sklad.zawodnik2
  or sklad.zawodnik1 = sklad.zawodnik3
  or sklad.zawodnik1 = sklad.zawodnik4
  or sklad.zawodnik1 = sklad.zawodnik5
  or sklad.zawodnik1 = sklad.zawodnik6
  or sklad.zawodnik2 = sklad.zawodnik3
  or sklad.zawodnik2 = sklad.zawodnik4
  or sklad.zawodnik2 = sklad.zawodnik5
  or sklad.zawodnik2 = sklad.zawodnik6
  or sklad.zawodnik3 = sklad.zawodnik4
  or sklad.zawodnik3 = sklad.zawodnik5
  or sklad.zawodnik3 = sklad.zawodnik6
  or sklad.zawodnik4 = sklad.zawodnik5
  or sklad.zawodnik4 = sklad.zawodnik6
  or sklad.zawodnik5 = sklad.zawodnik6
  then return 7;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik1 into d1;
  if d1 != sklad.druzyna
  then return 1;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik2 into d2;
  if d2 != sklad.druzyna
  then return 2;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik3 into d3;
  if d3 != sklad.druzyna
  then return 3;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik4 into d4;
  if d4 != sklad.druzyna
  then return 4;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik5 into d5;
  if d5 != sklad.druzyna
  then return 5;
  end if;
  select druzyna from zawodnicy where id = sklad.zawodnik6 into d6;
  if d6 != sklad.druzyna
  then return 6;
  end if;
  return 0;
end;
$$ language plpgsql;

create or replace function weryfikuj_wyniki(integer) returns boolean as $$
declare
  set sety%rowtype;
  pop_nrsetu numeric(1) := 1;
  correct_rows numeric(1) := 0;
  rows_in_table numeric(1);
  wyg_sety1 numeric(1) := 0;
  wyg_sety2 numeric(1) := 0;
begin
  for set in select * from sety where idmeczu = $1 order by nrsetu
  loop
    if pop_nrsetu != set.nrsetu
    then return false;
    else pop_nrsetu := pop_nrsetu + 1;
    end if;
    if set.wynik1 = 21
    then wyg_sety1 := wyg_sety1 + 1;
    else wyg_sety2 := wyg_sety2 + 1;
    end if;
    correct_rows := correct_rows + 1;
    exit when wyg_sety1 = 3 or wyg_sety2 = 3;
  end loop;
  if wyg_sety1 < 3 and wyg_sety2 < 3
  then return false;
  end if;
  select count(*) into rows_in_table from sety where idmeczu = $1;
  return correct_rows = rows_in_table;
end;
$$ language plpgsql;

-- Po wprowadzeniu wszystkich setów do bazy danych należy odpalić tą funkcję.
-- Jeśli wynik setu jest poprawny, to funkcja zwróci true.
-- Jeśli nie, to usunie go z bazy i zwróci false;

create or replace function weryfikuj_mecz(integer) returns boolean as $$
declare
  s1 integer;
  s2 integer;
  d1 varchar(30);
  d2 varchar(30);
begin
  select sklad1, sklad2 into s1, s2 from mecze where id = $1;
  select druzyna into d1 from sklady where id = s1;
  select druzyna into d2 from sklady where id = s2;
  if weryfikuj_wyniki($1) and d1 != d2
  then return true;
  else
    delete from sety where idmeczu = $1;
    delete from mecze where id = $1;
    return false;
  end if;
end;
$$ language plpgsql;