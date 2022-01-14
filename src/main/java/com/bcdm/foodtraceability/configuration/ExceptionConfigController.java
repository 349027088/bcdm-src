package com.bcdm.foodtraceability.configuration;

import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.view.json.MappingJackson2JsonView;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;

@ControllerAdvice
@Slf4j
public class ExceptionConfigController {

    // 专门用来捕获和处理Controller层的业务异常
    @ExceptionHandler(ServiceBusinessException.class)
    public ModelAndView serviceBusinessExceptionHandler(ServiceBusinessException e){
        log.info(e.getErrorMessage());
        ModelAndView mv = new ModelAndView(new MappingJackson2JsonView());
        mv.addObject("errorCode",e.getErrorCode());
        mv.addObject("message",e.getErrorMessage());
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
        ModelAndView mv = new ModelAndView(new MappingJackson2JsonView());
        mv.addObject("errorCode",false);
        mv.addObject("message","请求发生了空指针异常，请稍后再试");
        return mv;
    }

    // 专门用来捕获和处理Controller层的运行时异常
    @ExceptionHandler(RuntimeException.class)
    public ModelAndView runtimeExceptionHandler(RuntimeException e){
        e.printStackTrace();
        ModelAndView mv = new ModelAndView(new MappingJackson2JsonView());
        mv.addObject("errorCode",false);
        mv.addObject("message","请求发生了运行时异常，请稍后再试");
        return mv;
    }

    // 专门用来捕获和处理Controller层的异常
    @ExceptionHandler(Exception.class)
    public ModelAndView exceptionHandler(Exception e){
        ModelAndView mv = new ModelAndView(new MappingJackson2JsonView());
        mv.addObject("errorCode",false);
        mv.addObject("message","请求发生了异常，请稍后再试");
        return mv;
    }
}
