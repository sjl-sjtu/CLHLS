# CLHLS
This is the code to analyze the risk factors for cognitive impairment in older adults using the Chinese Longitudinal Healthy Longevity Survey (CLHLS) dataset. The dataset is available at <https://agingcenter.duke.edu/CLHLS>. The paper has been published in *BMC Public Health* [1].

I extracted the demographic characteristics and longitudinal morphological (body mass index, BMI), behavioral (activities of daily living, ADL), emotional (subjective well-being), and cognitive indicators. Cognitive status was measured using the Chinese Mini-Mental State Examination (CMMSE), revised from the commonly used Mini-Mental State Examination (MMSE). ADL was assessed with Katz scale on participants' ability to perform the daily activities of bathing, dressing, toileting, indoor mobility, bowel control, and eating. SWB was measured using a five-point Likert scale with eight items covering life satisfaction, positive emotions (optimism, happiness, personal control, and conscientiousness), and negative emotions (anxiety, loneliness, and uselessness).

I employed the following approaches to analyze the data:
* time-invariant and time-dependent Cox proportional hazard model for survival analysis
* linear and non-parametric mixed-effect model
* repeated measures correlation
* Bayesian joint model
* dynamic-DeepHit (a deep survival model)
* marginal structural Cox model for longitudinal causal inference

### Reference
[1] Sun, J., Deng, L., Li, Q. et al. Dynamic relations between longitudinal morphological, behavioral, and emotional indicators and cognitive impairment: evidence from the Chinese Longitudinal Healthy Longevity Survey. *BMC Public Health* **24**, 3516 (2024). <https://doi.org/10.1186/s12889-024-21072-w>
