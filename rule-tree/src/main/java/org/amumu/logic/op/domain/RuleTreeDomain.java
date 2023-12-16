package org.amumu.logic.op.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import org.amumu.logic.op.client.model.req.RuleTreeRequest;
import org.amumu.logic.op.infra.functions.ruletree.ConditionParser;

import java.util.Date;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RuleTreeDomain {
    private Long id;
    private Long version;
    private String bizCode;
    private String bizName;
    private RuleTreeConditionDomain condition;
    private Date gmtCreate;
    private Date gmtModified;
    private Long userId;
    private String userName;
    private String desc;

    public void init() {
        this.version = 0L;
        this.setBizName("bizName");
        this.setUserName("username");
    }

    public void upgrade(RuleTreeRequest ruleTreeRequest) {
        this.version = this.version+1;
        this.condition = ConditionParser.convert2Condition(ruleTreeRequest.getCondition());
    }
}
