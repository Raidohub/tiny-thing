package org.amumu.rule.tree.client.model.req;

import lombok.Data;

@Data
public class RuleTreeReq {
    private Long id;
    private String bizCode;
    private String condition;
    private Long userId;
    private String desc;

}
