package org.amumu.logic.op.domain;

import lombok.Data;

import java.util.List;

@Data
public class ExpressionConditionDomain {

    private String id;
    private String name;
    private String type;
    private String field;
    private String op;
    private List<String> val;
    private String conditions;
}
