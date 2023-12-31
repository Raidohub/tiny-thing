package org.amumu.rule.tree.client;

import java.util.Arrays;

public enum RuleTreeEnum {

    CONDITION("condition"),
    ENABLED("enabled");

    private String name;

    RuleTreeEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public enum OperatorEnum {
        EQ("eq"),
        NEQ("neq"),
        GT("gt"),
        GTE("gte"),
        LT("lt"),
        LET("lte"),
        IN("in"),
        NOT_IN("not in"),
        LIKE("like"),
        NOT_LIKE("not like");
        private String name;

        OperatorEnum(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }
    }

    public enum LogicalOperationEnum {
        AND("and"),
        OR("or"),
        NOT("not");
        private String name;

        LogicalOperationEnum(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public static LogicalOperationEnum enumValueOf(String name) {
            LogicalOperationEnum[] enums = LogicalOperationEnum.values();
            for(LogicalOperationEnum enumTmp : enums){
                if(enumTmp.getName().equals(name)){
                    return enumTmp;
                }
            }
            return null;
        }
    }
}
