package org.amumu.logic.op.domain.service;

import lombok.extern.slf4j.Slf4j;
import org.amumu.logic.op.client.model.req.ExpressionParamReq;
import org.amumu.logic.op.client.model.req.ExpressionRequest;
import org.amumu.logic.op.client.service.ExpressionService;
import org.amumu.logic.op.domain.ExpressionDomain;
import org.amumu.logic.op.domain.mapper.BeanMapper;
import org.amumu.logic.op.domain.model.ExpressionParam;
import org.amumu.logic.op.domain.repo.ExpressionRepoService;
import org.amumu.logic.op.infra.dao.model.ExpressionDO;
import org.amumu.logic.op.infra.utils.ConditionParser;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class ExpressionServiceImpl implements ExpressionService {

    @Autowired
    private ExpressionRepoService expressionRepoService;

    @Override
    public Integer createCondition(ExpressionRequest expressionRequest) {
        ExpressionDomain expressionDomain = BeanMapper.EXPRESSION_INSTANCE.req2Domain(expressionRequest);
        expressionDomain.init();

        ExpressionDO expressionDO = BeanMapper.EXPRESSION_INSTANCE.domain2DO(expressionDomain);
        return expressionRepoService.insert(expressionDO);
    }

    @Override
    public Integer updateCondition(ExpressionRequest expressionRequest) {
        ExpressionDO expressionDO = expressionRepoService.selectById(expressionRequest.getId());
        if (expressionDO == null) {
            log.info("expression not exist");
            return null;
        }

        ExpressionDomain expressionDomain = BeanMapper.EXPRESSION_INSTANCE.do2Domain(expressionDO);
        expressionDomain.upgrade(expressionRequest);
        ExpressionDO expressionDOUpgrade = BeanMapper.EXPRESSION_INSTANCE.domain2DO(expressionDomain);
        return expressionRepoService.insert(expressionDOUpgrade);
    }

    @Override
    public List<ExpressionDomain> selectList() {
        List<ExpressionDO> expressionDOList = expressionRepoService.selectAll();
        if (expressionDOList.isEmpty()) {
            log.info("expressionDOList is empty");
            return Collections.emptyList();
        }

        return BeanMapper.EXPRESSION_INSTANCE.do2Domain(expressionDOList);
    }

    @Override
    public Boolean parser(ExpressionParamReq paramReq) {
        if (paramReq == null) {
            log.info("request is null");
            return null;
        }
        ExpressionDO expressionDO = expressionRepoService.selectById(1L);
        if (expressionDO == null) {
            log.info("expressionDOList is empty");
            return null;
        }
        return ConditionParser.parser(expressionDO.getCondition(), paramReq);
    }
}
