library(caret)

setwd("D:/Dokumente/Studium/Master/Université de Genève/Kurse/Advanced Data Driven Decision Making/Case Study I-20230311")

prod_data = fread("Case_Stud_I.csv")

ggplot(data, aes(x = number_of_solar_panels, y = manufacturing_cost)) +
  geom_point() +
  labs(title = "Manufacturing costs and number of solar panels") +
  theme_bw()

prod_data$log_number = log(prod_data$number_of_solar_panels)

ggplot(prod_data, aes(x = log_number, y = manufacturing_cost)) +
  geom_point() +
  labs(title = "Manufacturing costs and number of solar panels") +
  theme_bw()

lin_model = lm(manufacturing_cost ~ log_number, data = prod_data)

lm(manufacturing_cost ~ log_number, prod_data)
par(mfrow = c(1,1))
plot(data$log_number, data$manufacturing_cost)
abline(1916.1, -149.1)

# Accuracy
plot(lin_model)
prod_data = data.frame(prod_data)
#x = data.frame(data$log_number)
prod_data$predictions = predict(lin_model, data = prod_data$log_number)
prod_data$deviation = prod_data$manufacturing_cost - prod_data$predictions
plot(prod_data$log_number, prod_data$deviation)

rmse(prod_data$manufacturing_cost, prod_data$predictions)

3.
estimates = data.frame(
  "number" = numeric(4),
  "log_number" = numeric(4),
  "estimate" = numeric(4))

estimates[1,1] = 4700
estimates[2,1] = 4800
estimates[3,1] = 4900
estimates[4,1] = 5000

estimates$log_number = log(estimates$number)

estimates$estimate = predict(lin_model, data.frame(log_number = estimates$log_number))

mean(estimates$estimate)

4.
prediction400 = 400 * mean(estimates$estimate)
prediction400

