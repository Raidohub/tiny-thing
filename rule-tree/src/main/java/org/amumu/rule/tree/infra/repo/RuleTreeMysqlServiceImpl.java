package org.amumu.rule.tree.infra.repo;

import org.amumu.rule.tree.domain.repo.RuleTreeRepoService;
import org.amumu.rule.tree.infra.dao.RuleTreeMapper;
import org.amumu.rule.tree.infra.dao.model.RuleTreeDO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Repository;

import java.util.List;

@Primary
@Repository("ruleTreeMysql")
public class RuleTreeMysqlServiceImpl implements RuleTreeRepoService {

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
