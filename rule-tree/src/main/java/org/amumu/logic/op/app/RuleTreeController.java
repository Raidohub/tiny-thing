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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@RestController
@RequestMapping("/api/v1/rule_tree/")
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

    @PostMapping("parser")
    public Result<Boolean> parser(@RequestBody RuleTreeParamReq paramReq) {
        return Result.ok(ruleTreeService.parser(paramReq));
    }

    @GetMapping("load")
    public Result<List<String>> load(String type) {
        if ("fields".equals(type)) {
            return Result.ok(Arrays.asList("name","age","gender"));
        } else if ("ops".equals(type)) {
            return Result.ok(Arrays.asList("=", "!=", ">", "≥", "<", "≤", "in", "not in", "like", "not like"));
        } else if ("logics".equals(type)) {
            return Result.ok(Arrays.asList("AND", "OR", "NOT"));
        }
        return Result.ok();
    }
}
