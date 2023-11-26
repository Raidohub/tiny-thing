package org.amumu.logic.op;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@MapperScan("org.amumu.logic.op.infra.dao")
public class LogicOpApplication {

    public static void main(String[] args) {
        SpringApplication.run(LogicOpApplication.class, args);
    }

}