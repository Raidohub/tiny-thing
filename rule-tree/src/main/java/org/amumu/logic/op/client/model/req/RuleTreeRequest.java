package org.amumu.logic.op.client.model.req;

import lombok.Data;

@Data
public class RuleTreeRequest {
    private Long id;
    private String bizCode;
    private String condition;
    private Long userId;
    private String desc;

}
