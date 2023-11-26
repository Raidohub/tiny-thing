package org.amumu.logic.op.client.model;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.amumu.logic.op.client.model.constants.ErrorCodeEnum;

import java.util.Map;

@ToString(callSuper = true)
public class Result<T> extends BaseResult {

    @Getter
    private T data;

    @Getter
    @Setter
    private Map<String, String> errorMap;

    public Result(int code, String message, T data) {
        super(code, message);
        this.data = data;
    }

    public static <T> Result<T> of(int code, String msg) {
        return new Result<>(code, msg, null);
    }

    public static <T> Result<T> error(int code, String msg) {
        return new Result<>(code, msg, null);
    }

    public static <T> Result<T> error(ErrorCodeEnum errorCode) {
        return error(errorCode.getCode(), errorCode.getMessage());
    }

    public static <T> Result<T> error(String msg) {
        return error(CODE_SYSTEM_ERROR, msg);
    }

    public static <T> Result<T> unauthorized(String msg) {
        return new Result<>(CODE_UNAUTHORIZED, msg, null);
    }

    public static <T> Result<T> fail(T fieldMessage) {
        return new Result<>(ErrorCodeEnum.BAD_REQUEST.getCode(), ErrorCodeEnum.BAD_REQUEST.getMessage(), fieldMessage);
    }

    public static Result<Object> fail(Map<String, String> errorMap) {
        Result<Object> result = Result.of(ErrorCodeEnum.BAD_REQUEST.getCode(), ErrorCodeEnum.BAD_REQUEST.getMessage());
        result.errorMap = errorMap;
        return result;
    }

    public static <T> Result<T> ok() {
        return ok(null);
    }

    public static <T> Result<T> ok(String msg) {
        return new Result<>(CODE_SUCCESS, msg, null);
    }

    public static <T> Result<T> ok(T data) {
        return new Result<>(CODE_SUCCESS, null, data);
    }

    public static <T> Result<T> delay(T data) {
        return ok(data);
    }
}
