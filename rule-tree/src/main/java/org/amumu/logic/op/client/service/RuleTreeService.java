package org.amumu.logic.op.client.service;

import org.amumu.logic.op.client.model.req.RuleTreeParamReq;
import org.amumu.logic.op.client.model.req.RuleTreeReq;
import org.amumu.logic.op.client.model.res.RuleTreeResultVO;
import org.amumu.logic.op.domain.RuleTreeDomain;

import java.util.List;

public interface RuleTreeService {
    Integer createCondition(RuleTreeReq ruleTreeReq);

    Integer updateCondition(RuleTreeReq ruleTreeReq);

    List<RuleTreeDomain> selectList();

    RuleTreeResultVO parser(RuleTreeParamReq paramReq);
}
