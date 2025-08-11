set serveroutput on

-- 반복문 for
-- 1씩 증가하면서 5회 반복
declare
    i number := 0;
begin
    for i in 1..5 loop
    dbms_output.put_line('i의 값=' || i);
    end loop;
end;
/
-- 10회 반복하면서 2의 배수 출력
declare
    i number := 0;
begin
    for i in 1..10 loop
        dbms_output.put_line('2의 배수=' || i*2);
    end loop;
end;
/
-- 1-10사이의 2의 배수만 출력
declare
    i number := 0;
begin
    for i in 1..10 loop
    if mod(i, 2) = 0 then
        dbms_output.put_line('2의 배수=' || i);
    end if;
    end loop;
end;
/
-- 반복문 while
declare
    i number := 0;
    result number := 0;
begin
    i := 1;
    while i <= 10 loop
        result := i *3;
        dbms_output.put_line(i || '*3=' || result);
        i := i + 1;
    end loop;
end;
/
-- 반복문 loop (무한반복)
declare
    v_num int := 0;
    v_sum int := 0;
begin
    v_num :=1;
    loop
        v_sum := v_sum + v_num;
        if v_num >= 10 then
            exit;
        end if;
        v_num := v_num +1;
    end loop;
    dbms_output.put_line('1~10 합:' || v_sum);
exception
    when others then 
        dbms_output.put_line(sqlcode || sqlerrm);
end;
/

-- loop문을 사용하여 : 1~100사이의 값중에서 3의 배수의 합 출력
declare -- 변수선언
    v_num int := 0;
    v_sum int := 0;
begin --plsql블럭
    v_num :=1;
    loop
        if mod(v_num, 3) = 0 then --v_num을 3으로 나누었을때 값이 0
            v_sum := v_sum + v_num;
        
        elsif v_num >= 100 then
            exit;
        end if;
        v_num := v_num +1;
    end loop;
         dbms_output.put_line('1~100사이의 값중에 3의 배수의 합:' || v_sum);
exception
    when others then       
        dbms_output.put_line(sqlcode || sqlerrm);
end;
/

-- cursor 간결한 표현 book_row도 
declare
begin
    for book_row in (select * from book where bk_pub='한빛미디어')
    loop
        if book_row.bk_author ='정호진' then
            dbms_output.put_line('hello 호진');
        end if;
        if book_row.bk_price > 20000 then
            dbms_output.put_line(book_row.bk_name || ':' ||
                book_row.bk_price || ':' || book_row.bk_author);
        end if;
    end loop;
end;
/

-- 쿼리 결과가 단일값

declare
    v_name varchar2(60);
    v_price varchar2(60);
begin
    select bk_name, bk_price into v_name, v_price from book
        where bk_author='이민수';
    dbms_output.put_line('책이름:' || v_name);
    dbms_output.put_line('책가격:' || v_price);
end;
/

declare
    cursor bk_cursor is select * from book where bk_pub='길벗';
    bk_buf book%rowtype;
begin
    open bk_cursor;
    loop
        fetch bk_cursor into bk_buf;
        exit when bk_cursor % notfound;
        dbms_output.put_line(bk_buf.bk_name||':'||bk_buf.bk_price);
    end loop;
    close bk_cursor;
end;
/


-- 프로시저: 프로그래밍에서 return이 없는 함수
create or replace procedure bk_info_p (
    p_bk_id in book.bk_id%type
) is
    v_bk_id book.bk_id%type;
    v_bk_price book.bk_price%type;
    v_bk_name book.bk_name%type;
begin
    select bk_id, bk_name, bk_price into v_bk_id, v_bk_name, v_bk_price
        from book where bk_id=p_bk_id;
    dbms_output.put_line('책번호:'|| v_bk_id);
    dbms_output.put_line('책이름:'|| v_bk_name);
    dbms_output.put_line('책가격:'|| v_bk_price);
exception
    when others then
        -- sqlcode:에러 코드, sqlerrm:에러 메시지
        dbms_output.put_line(sqlcode || sqlerrm);
end;
/

accept p_book_id prompt '책번호를 입력해주세요'
exec bk_info_p('&p_book_id');

----------------------------------

create or replace procedure bk_info_p2 (
    p_bk_name in book.bk_name%type
) is
    v_bk_id book.bk_id%type;
    v_bk_price book.bk_price%type;
    v_bk_name book.bk_name%type;
begin
    select bk_id, bk_name, bk_price into v_bk_id, v_bk_name, v_bk_price
        from book where bk_name=p_bk_name;
    dbms_output.put_line('책이름:'|| v_bk_name);
    dbms_output.put_line('책가격:'|| v_bk_price);
    dbms_output.put_line('책번호:'|| v_bk_id);
exception
    when others then
        -- sqlcode:에러 코드, sqlerrm:에러 메시지
        dbms_output.put_line(sqlcode || sqlerrm);
end;
/

accept p_book_name prompt '책이름을 입력해주세요'
exec bk_info_p2('&p_book_name');

-----------------------------------------------------------------
-- 출판사이름을 매개변수로 입력받아 해당 출판사의 책중 최대가격을 출력하는 프로시저
create or replace procedure bk_maxprice_p (
    p_bkpub in book.bk_pub%type
) is
    v_bk_price book.bk_price%type;
begin
    select max(bk_price) into v_bk_price
        from book where bk_pub=p_bkpub;    
    dbms_output.put_line(p_bkpub || ' 출판사 책의 최대가격:' || v_bk_price);
end;
/
accept p_bkpublic prompt '검색할 출판사를 입력하세요'
exec bk_maxprice_p('&p_bkpublic');

---------------------------------------------------------------

--출판사를 매개변수로 입력받아 책이름, 저자, 책가격, 수량, 출판일을 출력하는 프로시저
select bk_name, bk_author, bk_price, bk_quantity, bk_published from book
where bk_pub='길벗';

create or replace procedure bk_samepub_p (
    p_bk_pub in book.bk_pub%type
) is

begin
    for bk_buf in (select bk_name, bk_author, bk_price, bk_quantity, bk_published
        from book where bk_pub=p_bk_pub)
    loop
        dbms_output.put_line('책이름: ' || bk_buf.bk_name);
        dbms_output.put_line('저자: ' || bk_buf.bk_author);
        dbms_output.put_line('가격: ' || bk_buf.bk_price);
        dbms_output.put_line('수량: ' || bk_buf.bk_quantity);
        dbms_output.put_line('출판일: ' || bk_buf.bk_published);
        dbms_output.put_line('-------------------------------');
    end loop;
end;
/
accept p_pub prompt '책목록을 확인할 출판사를 입력해주세요'
exec bk_samepub_p('&p_pub');

-- 책이름과 저자를 매개변수로 받아서 책이름 또는 저자와 일치하는 출판사를
-- 구한 후 책이름, 출판일, 수량, 가격을 출력하는 프로시저
-- 조건1) 데이터가 없으면 '검색한 데이터가 존재하지 않습니다' 오류메시지 출력
select bk_name, bk_published, bk_quantity, bk_price from book 
    where bk_pub in (select bk_pub from book where bk_name='모던 자바스크립트' or bk_author='김찬우');

create or replace procedure book_find_p(
    p_bk_name in book.bk_name%type,
    p_bk_author in book.bk_author%type
) is
    chkNum int := 0;
begin
    for bk_buf in (
        select bk_name, bk_published, bk_quantity, bk_price from book 
            where bk_pub in (select bk_pub from book where bk_name=p_bk_name or bk_author=p_bk_author))
    loop
        dbms_output.put_line('책이름:'||bk_buf.bk_name);
        dbms_output.put_line('출판일:'||bk_buf.bk_published);
        dbms_output.put_line('수량:'||bk_buf.bk_quantity);
        dbms_output.put_line('가격:'||bk_buf.bk_price);
        dbms_output.put_line('----------------------');
        chkNum := 1;
    end loop;
    if chkNum = 0 then
        dbms_output.put_line('데이터가 존재하지 않습니다');
    end if;
end;
/
exec book_find_p('파','영');

----------------------------------------------------------

-- 함수(function): 프로그래밍에서 return이 있는 함수
-- 부서번호가 있는지 확인하는 함수
select count(*) from book where bk_quantity = 50 and rownum=1;

create or replace function bk_quantity_cnt_f (
    p_bk_q book.bk_quantity%type
) return number is
    cnt int;
begin
    select count(*) into cnt from book where bk_quantity=p_bk_q and rownum=1;
    if cnt > 0 then
        return cnt;
    else
        return 0;
    end if;
exception
    when others then
        dbms_output.put_line(sqlcode || sqlerrm);
        return -1;
end;
/
select distinct bk_quantity_cnt_f(50) from book;

---------------------------------------------

-- 책이름과 저자를 매개변수로 받아서 책이름 또는 저자명과 일치하는 출판사를
-- 구한 후 책이름, 출판일, 수량, 가격을 출력하는 프로시저
-- 조건1) 데이터가 없으면 '검색한 데이터가 존재하지 않습니다' 오류메시지 출력
-- 조건2) bk_pub_cnt_f 함수를 사용하여 조건1) 처리

create or replace procedure book_find3_p (
    p_bk_name in book.bk_name%type,
    p_bk_author in book.bk_author%type  -- 함수와 select문 두 곳에 매개변수가 쓰임
) is
    chkNum int := 0;
begin
    chkNum := bk_pub_cnt_f3(p_bk_name, p_bk_author);  -- 밑에 만든 함수와 매개변수가 chekNum에 쓰임
    if chkNum = 0 then
        dbms_output.put_line('데이터가 존재하지 않습니다');
    else
        for bk_buf in (
            select rownum, bk_name, bk_published, bk_quantity, bk_price
                from book where bk_pub in(select bk_pub from book
                    where bk_name=p_bk_name or bk_author=p_bk_author)) --매개변수
        loop
            dbms_output.put_line('책이름: ' || bk_buf.bk_name);
            dbms_output.put_line('출판일: ' || bk_buf.bk_published);
            dbms_output.put_line('수량: ' || bk_buf.bk_quantity);
            dbms_output.put_line('가격: ' || bk_buf.bk_price);
            dbms_output.put_line('-------------------------');
        end loop;
    end if;
end;
/
exec book_find3_p('코스모스','정수빈');




select count(*) from book
        where bk_name='코스모스' or bk_author='정수빈' and rownum=1;
        
create or replace function bk_pub_cnt_f3 (
    p_bk_name book.bk_name%type,
    p_bk_author book.bk_author%type
) return number is
    cnt int;
begin
    select count(*) into cnt from book 
        where (bk_name=p_bk_name or bk_author=p_bk_author) and rownum=1;
    return cnt;
exception
    when others then
        dbms_output.put_line(sqlcode || sqlerrm);
        return -1;
end;
/
    
select distinct bk_pub_cnt_f3('코스모스','정수빈') from book;
