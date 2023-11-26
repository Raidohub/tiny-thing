package org.amumu.logic.op.infra.repo;

import org.amumu.logic.op.domain.repo.RuleTreeRepoService;
import org.amumu.logic.op.infra.dao.RuleTreeMapper;
import org.amumu.logic.op.infra.dao.model.RuleTreeDO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class RuleTreeRepoServiceImpl implements RuleTreeRepoService {

    @Autowired
    private RuleTreeMapper ruleTreeMapper;

    @Override
    public List<RuleTreeDO> selectAll() {
        return ruleTreeMapper.selectAll();
    }

    @Override
    public RuleTreeDO selectById(Long id) {
        return ruleTreeMapper.selectOneById(id);
    }

    @Override
    public int insert(RuleTreeDO ruleTreeDO) {
        return ruleTreeMapper.insert(ruleTreeDO);
    }
}
