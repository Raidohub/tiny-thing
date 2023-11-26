package org.amumu.logic.op.client.service;

import org.amumu.logic.op.client.model.req.RuleTreeParamReq;
import org.amumu.logic.op.client.model.req.RuleTreeRequest;
import org.amumu.logic.op.domain.RuleTreeDomain;

import java.util.List;

public interface RuleTreeService {
    Integer createCondition(RuleTreeRequest ruleTreeRequest);

    Integer updateCondition(RuleTreeRequest ruleTreeRequest);

    List<RuleTreeDomain> selectList();

    Boolean parser(RuleTreeParamReq paramReq);
}
