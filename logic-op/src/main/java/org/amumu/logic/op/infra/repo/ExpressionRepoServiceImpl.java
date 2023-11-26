package org.amumu.logic.op.infra.repo;

import org.amumu.logic.op.domain.repo.ExpressionRepoService;
import org.amumu.logic.op.infra.dao.ExpressionMapper;
import org.amumu.logic.op.infra.dao.model.ExpressionDO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class ExpressionRepoServiceImpl implements ExpressionRepoService {

    @Autowired
    private ExpressionMapper expressionMapper;

    @Override
    public List<ExpressionDO> selectAll() {
        return expressionMapper.selectAll();
    }

    @Override
    public ExpressionDO selectById(Long id) {
        return expressionMapper.selectOneById(id);
    }

    @Override
    public int insert(ExpressionDO expressionDO) {
        return expressionMapper.insert(expressionDO);
    }
}
