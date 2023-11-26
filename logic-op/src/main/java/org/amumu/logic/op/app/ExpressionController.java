package org.amumu.logic.op.app;

import org.amumu.logic.op.client.model.Result;
import org.amumu.logic.op.client.model.req.ExpressionRequest;
import org.amumu.logic.op.client.model.res.ExpressionVO;
import org.amumu.logic.op.client.service.ExpressionService;
import org.amumu.logic.op.domain.ExpressionDomain;
import org.amumu.logic.op.domain.mapper.BeanMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Controller
@RestController
public class ExpressionController {

    @Autowired
    private ExpressionService expressionService;

    @GetMapping("render")
    public Result<List<ExpressionVO>> render(){
        List<ExpressionDomain> expressionDomainList = expressionService.selectList();
        if (!expressionDomainList.isEmpty()) {
            List<ExpressionVO> expressionVOList = BeanMapper.EXPRESSION_INSTANCE.domain2VO(expressionDomainList);
            return Result.ok(expressionVOList);
        }
        return Result.ok();
    }

    @PostMapping("create_condition")
    public Result<Integer> createCondition(ExpressionRequest expressionRequest) {
        if (expressionRequest == null || StringUtils.isBlank(expressionRequest.getCondition())) {
            return Result.ok(null);
        }
        return Result.ok(expressionService.createCondition(expressionRequest));
    }

    @GetMapping("parser")
    public Result<Boolean> parser(String param) {
        return Result.ok(expressionService.parser(param));
    }

}
