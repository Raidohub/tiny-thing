package org.amumu.rule.tree;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
@MapperScan("org.amumu.rule.tree.infra.dao")
public class RuleTreeApplication {

    public static void main(String[] args) {
        SpringApplication.run(RuleTreeApplication.class, args);
    }

}