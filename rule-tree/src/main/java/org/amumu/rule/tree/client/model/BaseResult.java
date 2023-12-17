package org.amumu.rule.tree.client.model;

import lombok.Getter;
import lombok.ToString;

/**
 * @author robotto
 * @version 1.0
 * @date 2022/4/16 23:15
 **/
@ToString
public class BaseResult {

    /**
     * 通用业务请求状态码
     */
    protected static final Integer CODE_SUCCESS = 200;
    protected static final Integer CODE_UNAUTHORIZED = 401;
    protected static final Integer CODE_SYSTEM_ERROR = 500;
    /**
     * 通用请求信息
     */
    public static final String SYSTEM_ERROR = "系统错误";
    public static final String MESSAGE_SUCCESS = "请求成功";
    public static final String QUERY_SUCCESS = "查询成功";
    public static final String INSERT_SUCCESS = "新增成功";
    public static final String UPDATE_SUCCESS = "更新成功";
    public static final String DELETE_SUCCESS = "删除成功";
    public static final String IMPORT_SUCCESS = "导入成功";
    public static final String EXPORT_SUCCESS = "导出成功";
    public static final String DOWNLOAD_SUCCESS = "下载成功";

    @Getter
    protected int code;

    @Getter
    protected String msg;

    public BaseResult() {}

    public BaseResult(Integer code, String msg) {
        this.code = code;
        this.msg = msg;
    }
}
