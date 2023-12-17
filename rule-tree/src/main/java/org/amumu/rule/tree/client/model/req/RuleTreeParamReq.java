package org.amumu.rule.tree.client.model.req;

import lombok.Data;
import org.amumu.rule.tree.domain.model.RuleTreeParam;

@Data
public class RuleTreeParamReq extends RuleTreeParam {

    private String condition;


}
