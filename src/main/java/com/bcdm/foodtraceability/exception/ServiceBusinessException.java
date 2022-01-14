package com.bcdm.foodtraceability.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ServiceBusinessException extends Exception{

    /**
     * 异常码
     */
    private String errorCode;

    /**
     * 异常信息
     */
    private String errorMessage;

}
