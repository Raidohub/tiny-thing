package org.amumu.logic.op.client.service;

import org.amumu.logic.op.client.model.req.ExpressionParamReq;
import org.amumu.logic.op.client.model.req.ExpressionRequest;
import org.amumu.logic.op.domain.ExpressionDomain;

import java.util.List;

public interface ExpressionService {
    Integer createCondition(ExpressionRequest expressionRequest);

    Integer updateCondition(ExpressionRequest expressionRequest);

    List<ExpressionDomain> selectList();

    Boolean parser(ExpressionParamReq paramReq);
}
