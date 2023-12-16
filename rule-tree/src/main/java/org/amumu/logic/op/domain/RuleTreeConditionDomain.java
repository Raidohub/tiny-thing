package org.amumu.logic.op.domain;

import com.fasterxml.jackson.databind.JsonNode;
import lombok.Data;

import java.util.List;

@Data
public class RuleTreeConditionDomain {

    private String id;
    private String name;
    private String type;
    private String field;
    private String op;
    private List<String> val;
    private JsonNode conditions;
}
