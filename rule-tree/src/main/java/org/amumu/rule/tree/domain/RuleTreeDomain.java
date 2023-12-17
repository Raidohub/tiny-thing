package org.amumu.rule.tree.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import org.amumu.rule.tree.client.model.req.RuleTreeReq;
import org.amumu.rule.tree.infra.functions.ruletree.ConditionParser;

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

    public void upgrade(RuleTreeReq ruleTreeReq) {
        this.version = this.version+1;
        this.condition = ConditionParser.convert2Condition(ruleTreeReq.getCondition());
    }
}
