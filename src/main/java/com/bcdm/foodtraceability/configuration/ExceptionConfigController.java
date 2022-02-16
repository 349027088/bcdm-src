package com.bcdm.foodtraceability.configuration;

import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.json.MappingJackson2JsonView;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SERVER_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 全局异常捕捉器
 * </p>
 *
 * @author 王
 * @since 2022-01-16
 */
@ControllerAdvice
@Slf4j
public class ExceptionConfigController {

    // 专门用来捕获和处理Controller层的业务异常
    @ExceptionHandler(ServiceBusinessException.class)
    public ModelAndView serviceBusinessExceptionHandler(ServiceBusinessException e){
        BlogAction.logger.info(e.getHttpMessage());
        ModelAndView mv = new ModelAndView(new MappingJackson2JsonView());
        mv.addObject("httpStatus",e.getHttpStatus());
        mv.addObject("httpMessage",e.getHttpMessage());
        return mv;
    }

    // 专门用来捕获和处理Controller层的验证异常
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ModelAndView methodArgumentNotValidExceptionHandler(MethodArgumentNotValidException e){
        return serviceBusinessExceptionHandler(new ServiceBusinessException(HTTP_RETURN_FAIL,e.getBindingResult().getAllErrors().iterator().next().getDefaultMessage()));
    }

    // 专门用来捕获和处理Controller层的空指针异常
    @ExceptionHandler(NullPointerException.class)
    public ModelAndView nullPointerExceptionHandler(NullPointerException e){
        e.printStackTrace();
        return serviceBusinessExceptionHandler(new ServiceBusinessException(HTTP_RETURN_SERVER_FAIL,NOT_POINT_ERROR));
    }

    // 专门用来捕获和处理Controller层的运行时异常
    @ExceptionHandler(RuntimeException.class)
    public ModelAndView runtimeExceptionHandler(RuntimeException e){
        e.printStackTrace();
        return serviceBusinessExceptionHandler(new ServiceBusinessException(HTTP_RETURN_SERVER_FAIL,RUNTIME_ERROR));
    }

    // 专门用来捕获和处理Controller层的异常
    @ExceptionHandler(Exception.class)
    public ModelAndView exceptionHandler(Exception e){
        e.printStackTrace();
        return serviceBusinessExceptionHandler(new ServiceBusinessException(HTTP_RETURN_SERVER_FAIL,SEVER_ERROR));
    }
}
