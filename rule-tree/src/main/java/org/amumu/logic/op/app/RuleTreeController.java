package org.amumu.logic.op.app;

import org.amumu.logic.op.client.model.Result;
import org.amumu.logic.op.client.model.req.RuleTreeParamReq;
import org.amumu.logic.op.client.model.req.RuleTreeRequest;
import org.amumu.logic.op.client.model.res.RuleTreeVO;
import org.amumu.logic.op.client.service.RuleTreeService;
import org.amumu.logic.op.domain.RuleTreeDomain;
import org.amumu.logic.op.domain.mapper.BeanMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin
@RequestMapping("/api/v1/rule_tree")
public class RuleTreeController {

    @Autowired
    private RuleTreeService ruleTreeService;

    @GetMapping("render")
    public Result<List<RuleTreeVO>> render(){
        List<RuleTreeDomain> ruleTreeDomainList = ruleTreeService.selectList();
        if (!ruleTreeDomainList.isEmpty()) {
            List<RuleTreeVO> ruleTreeVOList = BeanMapper.RULE_TREE_INSTANCE.domain2VO(ruleTreeDomainList);
            return Result.ok(ruleTreeVOList);
        }
        return Result.ok();
    }

    @PostMapping("create_condition")
    public Result<Integer> createCondition(RuleTreeRequest ruleTreeRequest) {
        if (ruleTreeRequest == null || StringUtils.isBlank(ruleTreeRequest.getCondition())) {
            return Result.ok(null);
        }
        return Result.ok(ruleTreeService.createCondition(ruleTreeRequest));
    }

    @GetMapping("parser")
    public Result<Boolean> parser(@RequestBody RuleTreeParamReq paramReq) {
        return Result.ok(ruleTreeService.parser(paramReq));
    }

}
