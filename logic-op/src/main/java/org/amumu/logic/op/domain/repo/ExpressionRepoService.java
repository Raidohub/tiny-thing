package org.amumu.logic.op.domain.repo;

import org.amumu.logic.op.infra.dao.model.ExpressionDO;

import java.util.List;

public interface ExpressionRepoService {
    List<ExpressionDO> selectAll();

    ExpressionDO selectById(Long id);

    int insert(ExpressionDO expressionDO);
}
